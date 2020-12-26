%% -*- erlang-indent-level: 2 -*-
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%%-----------------------------------------------------------------------
%% File        : typer.erl
%% Author(s)   : The first version of typer was written by Bingwen He
%%               with guidance from Kostis Sagonas and Tobias Lindahl.
%%               Since June 2008 typer is maintained by Kostis Sagonas.
%% Description : An Erlang/OTP application that shows type information
%%               for Erlang modules to the user.  Additionally, it can
%%               annotate the code of files with such type information.
%%-----------------------------------------------------------------------

-module(els_typer).

-export([ start/1 ]).

-type files()      :: [file:filename()].
-type callgraph()  :: dialyzer_callgraph:callgraph().
-type codeserver() :: dialyzer_codeserver:codeserver().
-type plt()        :: dialyzer_plt:plt().

-record(analysis,
        {mode       = 'show',
         macros     = []     :: [{atom(), term()}],
         includes   = []     :: files(),
         codeserver = dialyzer_codeserver:new():: codeserver(),
         callgraph  = dialyzer_callgraph:new() :: callgraph(),
         files      = []     :: files(),   % absolute names
         plt        = none     :: 'none' | file:filename(),
         show_succ  = false              :: boolean(),
         %% Files in 'fms' are compilable with option 'to_pp'; we keep them
         %% as {FileName, ModuleName} in case the ModuleName is different
         fms        = []     :: [{file:filename(), module()}],
         ex_func    = map__new()   :: map_dict(),
         record     = map__new()   :: map_dict(),
         func       = map__new()   :: map_dict(),
         inc_func   = map__new()   :: map_dict(),
         trust_plt  = dialyzer_plt:new() :: plt()}).
-type analysis() :: #analysis{}.

start(Uri) ->
  Path = binary_to_list(els_uri:path(Uri)),
  Analysis = #analysis{
                %% TODO: Pass macros
                macros = []
                %% TODO: Pass includes
               , includes = []
                %% TODO: Dependencies (eg for parse transforms)
               },
  Timer = dialyzer_timing:init(false),
  TrustedFiles = [], %% TODO
  Analysis2 = extract(Analysis, TrustedFiles),
  Analysis3 = Analysis2#analysis{files = [Path]},
  Analysis4 = collect_info(Analysis3),
  TypeInfo = get_type_info(Analysis4),
  dialyzer_timing:stop(Timer),
  show(TypeInfo),
  ok.

%%--------------------------------------------------------------------

-spec extract(analysis(), files()) -> analysis().

extract(#analysis{macros = Macros,
                  includes = Includes,
                  trust_plt = TrustPLT} = Analysis, TrustedFiles) ->
  %% io:format("--- Extracting trusted typer_info... "),
  Ds = [{d, Name, Value} || {Name, Value} <- Macros],
  CodeServer = dialyzer_codeserver:new(),
  Fun =
    fun(File, CS) ->
        %% We include one more dir; the one above the one we are trusting
        %% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
        %% /home/tests/ rather than /home/tests/typer_ann/
        AllIncludes = [filename:dirname(filename:dirname(File)) | Includes],
        Is = [{i, Dir} || Dir <- AllIncludes],
        CompOpts = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
        case dialyzer_utils:get_core_from_src(File, CompOpts) of
          {ok, Core} ->
            case dialyzer_utils:get_record_and_type_info(Core) of
              {ok, RecDict} ->
                Mod = list_to_atom(filename:basename(File, ".erl")),
                case dialyzer_utils:get_spec_info(Mod, Core, RecDict) of
                  {ok, SpecDict, CbDict} ->
                    CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
                    dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CbDict, CS1);
                  {error, Reason} -> compile_error([Reason])
                end;
              {error, Reason} -> compile_error([Reason])
            end;
          {error, Reason} -> compile_error(Reason)
        end
    end,
  CodeServer1 = lists:foldl(Fun, CodeServer, TrustedFiles),
  %% Process remote types
  NewCodeServer =
    try
      CodeServer2 =
        dialyzer_utils:merge_types(CodeServer1,
                                   TrustPLT), % XXX change to the PLT?
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(CodeServer1),
      case sets:size(NewExpTypes) of 0 -> ok end,
      CodeServer3 = dialyzer_codeserver:finalize_exported_types(NewExpTypes, CodeServer2),
      CodeServer4 = dialyzer_utils:process_record_remote_types(CodeServer3),
      dialyzer_contracts:process_contract_remote_types(CodeServer4)
    catch
      throw:{error, ErrorMsg} ->
        compile_error(ErrorMsg)
    end,
  %% Create TrustPLT
  ContractsDict = dialyzer_codeserver:get_contracts(NewCodeServer),
  Contracts = orddict:from_list(dict:to_list(ContractsDict)),
  NewTrustPLT = dialyzer_plt:insert_contract_list(TrustPLT, Contracts),
  Analysis#analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

-spec get_type_info(analysis()) -> analysis().

get_type_info(#analysis{callgraph = CallGraph,
                        trust_plt = TrustPLT,
                        codeserver = CodeServer} = Analysis) ->
  StrippedCallGraph = remove_external(CallGraph, TrustPLT),
  %% io:format("--- Analyzing callgraph... "),
  try
    NewPlt = dialyzer_succ_typings:analyze_callgraph(StrippedCallGraph,
                                                     TrustPLT,
                                                     CodeServer),
    Analysis#analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}
  catch
    error:What:Stacktrace ->
      fatal_error(io_lib:format("Analysis failed with message: ~tp",
                                [{What, Stacktrace}]));
    throw:{dialyzer_succ_typing_error, Msg} ->
      fatal_error(io_lib:format("Analysis failed with message: ~ts", [Msg]))
  end.

-spec remove_external(callgraph(), plt()) -> callgraph().

remove_external(CallGraph, PLT) ->
  {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
  case get_external(Ext, PLT) of
    [] -> ok;
    Externals ->
      msg(io_lib:format(" Unknown functions: ~tp\n", [lists:usort(Externals)])),
      ExtTypes = rcv_ext_types(),
      case ExtTypes of
        [] -> ok;
        _ -> msg(io_lib:format(" Unknown types: ~tp\n", [ExtTypes]))
      end
  end,
  StrippedCG0.

-spec get_external([{mfa(), mfa()}], plt()) -> [mfa()].

get_external(Exts, Plt) ->
  Fun = fun ({_From, To = {M, F, A}}, Acc) ->
            case dialyzer_plt:contains_mfa(Plt, To) of
              false ->
                case erl_bif_types:is_known(M, F, A) of
                  true -> Acc;
                  false -> [To|Acc]
                end;
              true -> Acc
            end
        end,
  lists:foldl(Fun, [], Exts).

%%--------------------------------------------------------------------
%% Showing type information or annotating files with such information.
%%--------------------------------------------------------------------

-define(TYPER_ANN_DIR, "typer_ann").

-type line()      :: non_neg_integer().
-type fa()        :: {atom(), arity()}.
-type func_info() :: {line(), atom(), arity()}.

-record(info, {records = maps:new() :: erl_types:type_table(),
               functions = []       :: [func_info()],
               types = map__new()   :: map_dict()}).

show(Analysis) ->
  Fun = fun ({File, Module}) ->
            Info = get_final_info(File, Module, Analysis),
            show_type_info(Info)
        end,
  lists:foreach(Fun, Analysis#analysis.fms).

get_final_info(File, Module, Analysis) ->
  Records = get_records(File, Analysis),
  Types = get_types(Module, Analysis, Records),
  Functions = get_functions(File, Analysis),
  #info{records = Records, functions = Functions, types = Types}.

get_records(File, Analysis) ->
  map__lookup(File, Analysis#analysis.record).

get_types(Module, Analysis, Records) ->
  TypeInfoPlt = Analysis#analysis.trust_plt,
  TypeInfo =
    case dialyzer_plt:lookup_module(TypeInfoPlt, Module) of
      none -> [];
      {value, List} -> List
    end,
  CodeServer = Analysis#analysis.codeserver,
  TypeInfoList =
    case Analysis#analysis.show_succ of
      true ->
        [convert_type_info(I) || I <- TypeInfo];
      false ->
        [get_type(I, CodeServer, Records) || I <- TypeInfo]
    end,
  map__from_list(TypeInfoList).

convert_type_info({{_M, F, A}, Range, Arg}) ->
  {{F, A}, {Range, Arg}}.

get_type({{M, F, A} = MFA, Range, Arg}, CodeServer, Records) ->
  case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
    error ->
      {{F, A}, {Range, Arg}};
    {ok, {_FileLine, Contract, _Xtra}} ->
      Sig = erl_types:t_fun(Arg, Range),
      case dialyzer_contracts:check_contract(Contract, Sig) of
        ok -> {{F, A}, {contract, Contract}};
        {range_warnings, _} ->
          {{F, A}, {contract, Contract}};
        {error, {overlapping_contract, []}} ->
          {{F, A}, {contract, Contract}};
        {error, invalid_contract} ->
          CString = dialyzer_contracts:contract_to_string(Contract),
          SigString = dialyzer_utils:format_sig(Sig, Records),
          Msg = io_lib:format("Error in contract of function ~w:~tw/~w\n"
                              "\t The contract is: " ++ CString ++ "\n" ++
                                "\t but the inferred signature is: ~ts",
                              [M, F, A, SigString]),
          fatal_error(Msg);
        {error, ErrorStr} when is_list(ErrorStr) -> % ErrorStr is a string()
          Msg = io_lib:format("Error in contract of function ~w:~tw/~w: ~ts",
                              [M, F, A, ErrorStr]),
          fatal_error(Msg)
      end
  end.

get_functions(File, Analysis) ->
  Funcs = map__lookup(File, Analysis#analysis.func),
  Inc_Funcs = map__lookup(File, Analysis#analysis.inc_func),
  remove_module_info(Funcs) ++ normalize_incFuncs(Inc_Funcs).

normalize_incFuncs(Functions) ->
  [FunInfo || {_FileName, FunInfo} <- Functions].

-spec remove_module_info([func_info()]) -> [func_info()].

remove_module_info(FunInfoList) ->
  F = fun ({_,module_info,0}) -> false;
          ({_,module_info,1}) -> false;
          ({Line,F,A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
      end,
  lists:filter(F, FunInfoList).

get_type_string(F, A, Info) ->
  Type = get_type_info({F,A}, Info#info.types),
  TypeStr =
    case Type of
      {contract, C} ->
        dialyzer_contracts:contract_to_string(C);
      {RetType, ArgType} ->
        Sig = erl_types:t_fun(ArgType, RetType),
        dialyzer_utils:format_sig(Sig, Info#info.records)
    end,
  Prefix = lists:concat(["-spec ", erl_types:atom_to_string(F)]),
  lists:concat([Prefix, TypeStr, "."]).

show_type_info(Info) ->
  Fun = fun ({LineNo, F, A}) ->
            erlang:display({line_no, LineNo}),
            TypeInfo = get_type_string(F, A, Info),
            io:format("~ts\n", [TypeInfo])
        end,
  lists:foreach(Fun, Info#info.functions).

get_type_info(Func, Types) ->
  case map__lookup(Func, Types) of
    none ->
      %% Note: Typeinfo of any function should exist in
      %% the result offered by dialyzer, otherwise there
      %% *must* be something wrong with the analysis
      Msg = io_lib:format("No type info for function: ~tp\n", [Func]),
      fatal_error(Msg);
    {contract, _Fun} = C -> C;
    {_RetType, _ArgType} = RA -> RA
  end.

%%--------------------------------------------------------------------
%% File processing.
%%--------------------------------------------------------------------

-type inc_file_info() :: {file:filename(), func_info()}.

-record(tmpAcc, {file     :: file:filename(),
                 module     :: atom(),
                 funcAcc = []   :: [func_info()],
                 incFuncAcc = []  :: [inc_file_info()],
                 dialyzerObj = [] :: [{mfa(), {_, _}}]}).

-spec collect_info(analysis()) -> analysis().

collect_info(Analysis) ->
  NewPlt =
    try get_dialyzer_plt(Analysis) of
      DialyzerPlt ->
        dialyzer_plt:merge_plts([Analysis#analysis.trust_plt, DialyzerPlt])
    catch
      throw:{dialyzer_error,_Reason} ->
        fatal_error("Dialyzer's PLT is missing or is not up-to-date; please (re)create it")
    end,
  NewAnalysis = lists:foldl(fun collect_one_file_info/2,
                            Analysis#analysis{trust_plt = NewPlt},
                            Analysis#analysis.files),
  %% Process Remote Types
  TmpCServer = NewAnalysis#analysis.codeserver,
  NewCServer =
    try
      TmpCServer1 = dialyzer_utils:merge_types(TmpCServer, NewPlt),
      NewExpTypes = dialyzer_codeserver:get_temp_exported_types(TmpCServer),
      OldExpTypes = dialyzer_plt:get_exported_types(NewPlt),
      MergedExpTypes = sets:union(NewExpTypes, OldExpTypes),
      TmpCServer2 =
        dialyzer_codeserver:finalize_exported_types(MergedExpTypes, TmpCServer1),
      TmpCServer3 = dialyzer_utils:process_record_remote_types(TmpCServer2),
      dialyzer_contracts:process_contract_remote_types(TmpCServer3)
    catch
      throw:{error, ErrorMsg} ->
        fatal_error(ErrorMsg)
    end,
  NewAnalysis#analysis{codeserver = NewCServer}.

collect_one_file_info(File, Analysis) ->
  Ds = [{d,Name,Val} || {Name,Val} <- Analysis#analysis.macros],
  %% Current directory should also be included in "Includes".
  Includes = [filename:dirname(File)|Analysis#analysis.includes],
  Is = [{i,Dir} || Dir <- Includes],
  Options = dialyzer_utils:src_compiler_opts() ++ Is ++ Ds,
  case dialyzer_utils:get_core_from_src(File, Options) of
    {error, Reason} ->
      %% io:format("File=~tp\n,Options=~p\n,Error=~p\n", [File,Options,Reason]),
      compile_error(Reason);
    {ok, Core} ->
      case dialyzer_utils:get_record_and_type_info(Core) of
        {error, Reason} -> compile_error([Reason]);
        {ok, Records} ->
          Mod = cerl:concrete(cerl:module_name(Core)),
          case dialyzer_utils:get_spec_info(Mod, Core, Records) of
            {error, Reason} -> compile_error([Reason]);
            {ok, SpecInfo, CbInfo} ->
              ExpTypes = get_exported_types_from_core(Core),
              analyze_core_tree(Core, Records, SpecInfo, CbInfo,
                                ExpTypes, Analysis, File)
          end
      end
  end.

analyze_core_tree(Core, Records, SpecInfo, CbInfo, ExpTypes, Analysis, File) ->
  Module = cerl:concrete(cerl:module_name(Core)),
  TmpTree = cerl:from_records(Core),
  CS1 = Analysis#analysis.codeserver,
  NextLabel = dialyzer_codeserver:get_next_core_label(CS1),
  {Tree, NewLabel} = cerl_trees:label(TmpTree, NextLabel),
  CS2 = dialyzer_codeserver:insert(Module, Tree, CS1),
  CS3 = dialyzer_codeserver:set_next_core_label(NewLabel, CS2),
  CS4 = dialyzer_codeserver:store_temp_records(Module, Records, CS3),
  CS5 = dialyzer_codeserver:store_temp_contracts(Module, SpecInfo, CbInfo, CS4),
  OldExpTypes = dialyzer_codeserver:get_temp_exported_types(CS5),
  MergedExpTypes = sets:union(ExpTypes, OldExpTypes),
  CS6 = dialyzer_codeserver:insert_temp_exported_types(MergedExpTypes, CS5),
  Ex_Funcs = [{0,F,A} || {_,_,{F,A}} <- cerl:module_exports(Tree)],
  CG = Analysis#analysis.callgraph,
  {V, E} = dialyzer_callgraph:scan_core_tree(Tree, CG),
  dialyzer_callgraph:add_edges(E, V, CG),
  Fun = fun analyze_one_function/2,
  All_Defs = cerl:module_defs(Tree),
  Acc = lists:foldl(Fun, #tmpAcc{file = File, module = Module}, All_Defs),
  Exported_FuncMap = map__insert({File, Ex_Funcs}, Analysis#analysis.ex_func),
  %% we must sort all functions in the file which
  %% originate from this file by *numerical order* of lineNo
  Sorted_Functions = lists:keysort(1, Acc#tmpAcc.funcAcc),
  FuncMap = map__insert({File, Sorted_Functions}, Analysis#analysis.func),
  %% we do not need to sort functions which are imported from included files
  IncFuncMap = map__insert({File, Acc#tmpAcc.incFuncAcc},
                           Analysis#analysis.inc_func),
  FMs = Analysis#analysis.fms ++ [{File, Module}],
  RecordMap = map__insert({File, Records}, Analysis#analysis.record),
  Analysis#analysis{fms = FMs,
                    callgraph = CG,
                    codeserver = CS6,
                    ex_func = Exported_FuncMap,
                    inc_func = IncFuncMap,
                    record = RecordMap,
                    func = FuncMap}.

analyze_one_function({Var, FunBody} = Function, Acc) ->
  F = cerl:fname_id(Var),
  A = cerl:fname_arity(Var),
  TmpDialyzerObj = {{Acc#tmpAcc.module, F, A}, Function},
  NewDialyzerObj = Acc#tmpAcc.dialyzerObj ++ [TmpDialyzerObj],
  Anno = cerl:get_ann(FunBody),
  LineNo = get_line(Anno),
  FileName = get_file(Anno),
  BaseName = filename:basename(FileName),
  FuncInfo = {LineNo, F, A},
  OriginalName = Acc#tmpAcc.file,
  {FuncAcc, IncFuncAcc} =
    case (FileName =:= OriginalName) orelse (BaseName =:= OriginalName) of
      true -> %% Coming from original file
        %% io:format("Added function ~tp\n", [{LineNo, F, A}]),
        {Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
      false ->
        %% Coming from other sourses, including:
        %%     -- .yrl (yecc-generated file)
        %%     -- yeccpre.hrl (yecc-generated file)
        %%     -- other cases
        {Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
    end,
  Acc#tmpAcc{funcAcc = FuncAcc,
             incFuncAcc = IncFuncAcc,
             dialyzerObj = NewDialyzerObj}.

get_line([Line|_]) when is_integer(Line) -> Line;
get_line([_|T]) -> get_line(T);
get_line([]) -> none.

get_file([{file,File}|_]) -> File;
get_file([_|T]) -> get_file(T);
get_file([]) -> "no_file". % should not happen

-spec get_dialyzer_plt(analysis()) -> plt().

get_dialyzer_plt(#analysis{plt = PltFile0}) ->
  PltFile =
    case PltFile0 =:= none of
      true -> dialyzer_plt:get_default_plt();
      false -> PltFile0
    end,
  dialyzer_plt:from_file(PltFile).

%% Exported Types

get_exported_types_from_core(Core) ->
  Attrs = cerl:module_attrs(Core),
  ExpTypes1 = [cerl:concrete(L2) || {L1, L2} <- Attrs,
                                    cerl:is_literal(L1),
                                    cerl:is_literal(L2),
                                    cerl:concrete(L1) =:= 'export_type'],
  ExpTypes2 = lists:flatten(ExpTypes1),
  M = cerl:atom_val(cerl:module_name(Core)),
  sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

%%--------------------------------------------------------------------
%% Utilities for error reporting.
%%--------------------------------------------------------------------

-spec fatal_error(string()) -> no_return().

fatal_error(Slogan) ->
  msg(io_lib:format("typer: ~ts\n", [Slogan])),
  erlang:halt(1).

-spec compile_error([string()]) -> no_return().

compile_error(Reason) ->
  JoinedString = lists:flatten([X ++ "\n" || X <- Reason]),
  Msg = "Analysis failed with error report:\n" ++ JoinedString,
  fatal_error(Msg).

-spec msg(string()) -> 'ok'.

msg(Msg) ->
  io:format(standard_error, "~ts", [Msg]).

%%--------------------------------------------------------------------
%% Handle messages.
%%--------------------------------------------------------------------

rcv_ext_types() ->
  Self = self(),
  Self ! {Self, done},
  rcv_ext_types(Self, []).

rcv_ext_types(Self, ExtTypes) ->
  receive
    {Self, ext_types, ExtType} ->
      rcv_ext_types(Self, [ExtType|ExtTypes]);
    {Self, done} ->
      lists:usort(ExtTypes)
  end.

%%--------------------------------------------------------------------
%% A convenient abstraction of a Key-Value mapping data structure
%% specialized for the uses in this module
%%--------------------------------------------------------------------

-type map_dict() :: dict:dict().

-spec map__new() -> map_dict().
map__new() ->
  dict:new().

-spec map__insert({term(), term()}, map_dict()) -> map_dict().
map__insert(Object, Map) ->
  {Key, Value} = Object,
  dict:store(Key, Value, Map).

-spec map__lookup(term(), map_dict()) -> term().
map__lookup(Key, Map) ->
  try dict:fetch(Key, Map) catch error:_ -> none end.

-spec map__from_list([{fa(), term()}]) -> map_dict().
map__from_list(List) ->
  dict:from_list(List).
