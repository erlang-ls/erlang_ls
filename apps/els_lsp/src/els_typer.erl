%% =============================================================================
%% An erlang_ls fork of Erlang/OTP's typer
%% =============================================================================
%% The main reasons for the fork:
%%   * Typer is designed as a CLI tool
%%   * Typer does not allow to pass Macros and Includes options
%% =============================================================================
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

-export([get_info/1, get_type_spec/3]).

-include("els_lsp.hrl").

-type files() :: [file:filename()].
-type callgraph() :: dialyzer_callgraph:callgraph().
-type codeserver() :: dialyzer_codeserver:codeserver().
-type plt() :: dialyzer_plt:plt().

-record(analysis, {
    mode = 'show',
    macros = [] :: [{atom(), term()}],
    includes = [] :: files(),
    codeserver = dialyzer_codeserver:new() :: codeserver(),
    callgraph = dialyzer_callgraph:new() :: callgraph(),
    % absolute names
    files = [] :: files(),
    plt = none :: 'none' | file:filename(),
    show_succ = false :: boolean(),
    %% Files in 'fms' are compilable with option 'to_pp'; we keep them
    %% as {FileName, ModuleName} in case the ModuleName is different
    fms = [] :: [{file:filename(), module()}],
    ex_func = map__new() :: map_dict(),
    record = map__new() :: map_dict(),
    func = map__new() :: map_dict(),
    inc_func = map__new() :: map_dict(),
    trust_plt = dialyzer_plt:new() :: plt()
}).
-type analysis() :: #analysis{}.

-record(info, {
    records = maps:new() :: erl_types:type_table(),
    functions = [] :: [func_info()],
    types = map__new() :: map_dict()
}).
-type info() :: #info{}.
-export_type([info/0]).

-spec get_info(uri()) -> #info{}.
get_info(Uri) ->
    Path = binary_to_list(els_uri:path(Uri)),
    Macros = els_compiler_diagnostics:macro_options(),
    Includes = els_compiler_diagnostics:include_options(),
    Analysis = #analysis{
        macros = Macros,
        includes = Includes
    },
    TrustedFiles = [],
    Analysis2 = extract(Analysis, TrustedFiles),
    Analysis3 = Analysis2#analysis{files = [Path]},
    Analysis4 = collect_info(Analysis3),
    Analysis5 = get_type_info(Analysis4),
    [{File, Module}] = Analysis5#analysis.fms,
    get_final_info(File, Module, Analysis5).

%%--------------------------------------------------------------------

-spec extract(analysis(), files()) -> analysis().

extract(
    #analysis{
        macros = Macros,
        includes = Includes,
        trust_plt = TrustPLT
    } = Analysis,
    TrustedFiles
) ->
    CodeServer = dialyzer_codeserver:new(),
    Fun =
        fun(File, CS) ->
            %% We include one more dir; the one above the one we are trusting
            %% E.g, for /home/tests/typer_ann/test.ann.erl, we should include
            %% /home/tests/ rather than /home/tests/typer_ann/
            CompOpts = dialyzer_utils:src_compiler_opts() ++ Includes ++ Macros,
            {ok, Core} = dialyzer_utils:get_core_from_src(File, CompOpts),
            {ok, RecDict} = dialyzer_utils:get_record_and_type_info(Core),
            Mod = list_to_atom(filename:basename(File, ".erl")),
            {ok, SpecDict, CbDict} = dialyzer_utils:get_spec_info(Mod, Core, RecDict),
            CS1 = dialyzer_codeserver:store_temp_records(Mod, RecDict, CS),
            dialyzer_codeserver:store_temp_contracts(Mod, SpecDict, CbDict, CS1)
        end,
    CodeServer1 = lists:foldl(Fun, CodeServer, TrustedFiles),
    CodeServer2 =
        dialyzer_utils:merge_types(
            CodeServer1,
            % XXX change to the PLT?
            TrustPLT
        ),
    NewExpTypes = dialyzer_codeserver:get_temp_exported_types(CodeServer1),
    case sets:size(NewExpTypes) of
        0 -> ok
    end,
    CodeServer3 = dialyzer_codeserver:finalize_exported_types(NewExpTypes, CodeServer2),
    CodeServer4 = dialyzer_utils:process_record_remote_types(CodeServer3),
    NewCodeServer = dialyzer_contracts:process_contract_remote_types(CodeServer4),
    ContractsDict = dialyzer_codeserver:get_contracts(NewCodeServer),
    Contracts = orddict:from_list(dict:to_list(ContractsDict)),
    NewTrustPLT = dialyzer_plt:insert_contract_list(TrustPLT, Contracts),
    Analysis#analysis{trust_plt = NewTrustPLT}.

%%--------------------------------------------------------------------

-spec get_type_info(analysis()) -> analysis().

-if(?OTP_RELEASE >= 25).
get_type_info(
    #analysis{
        callgraph = CallGraph,
        trust_plt = TrustPLT,
        codeserver = CodeServer
    } = Analysis
) ->
    StrippedCallGraph = remove_external(CallGraph, TrustPLT),
    NewPlt = dialyzer_succ_typings:analyze_callgraph(
        StrippedCallGraph,
        TrustPLT,
        CodeServer,
        none,
        []
    ),
    Analysis#analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}.
-else.
get_type_info(
    #analysis{
        callgraph = CallGraph,
        trust_plt = TrustPLT,
        codeserver = CodeServer
    } = Analysis
) ->
    StrippedCallGraph = remove_external(CallGraph, TrustPLT),
    NewPlt = dialyzer_succ_typings:analyze_callgraph(
        StrippedCallGraph,
        TrustPLT,
        CodeServer
    ),
    Analysis#analysis{callgraph = StrippedCallGraph, trust_plt = NewPlt}.
-endif.

-spec remove_external(callgraph(), plt()) -> callgraph().

remove_external(CallGraph, PLT) ->
    {StrippedCG0, Ext} = dialyzer_callgraph:remove_external(CallGraph),
    case get_external(Ext, PLT) of
        [] -> ok;
        _Externals -> rcv_ext_types()
    end,
    StrippedCG0.

-spec get_external([{mfa(), mfa()}], plt()) -> [mfa()].

get_external(Exts, Plt) ->
    Fun = fun({_From, To = {M, F, A}}, Acc) ->
        case dialyzer_plt:contains_mfa(Plt, To) of
            false ->
                case erl_bif_types:is_known(M, F, A) of
                    true -> Acc;
                    false -> [To | Acc]
                end;
            true ->
                Acc
        end
    end,
    lists:foldl(Fun, [], Exts).

%%--------------------------------------------------------------------
%% Showing type information or annotating files with such information.
%%--------------------------------------------------------------------

-type fa() :: {atom(), arity()}.
-type func_info() :: {non_neg_integer(), atom(), arity()}.

-spec get_final_info(string(), atom(), #analysis{}) -> #info{}.
get_final_info(File, Module, Analysis) ->
    Records = get_records(File, Analysis),
    Types = get_types(Module, Analysis, Records),
    Functions = get_functions(File, Analysis),
    #info{records = Records, functions = Functions, types = Types}.

-spec get_records(string(), #analysis{}) -> any().
get_records(File, Analysis) ->
    map__lookup(File, Analysis#analysis.record).

-spec get_types(atom(), #analysis{}, any()) -> dict:dict().
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

-spec convert_type_info({mfa(), any(), any()}) -> {fa(), {any(), any()}}.
convert_type_info({{_M, F, A}, Range, Arg}) ->
    {{F, A}, {Range, Arg}}.

-spec get_type({mfa(), any(), any()}, any(), any()) -> {fa(), {any(), any()}}.
get_type({{_M, F, A} = MFA, Range, Arg}, CodeServer, _Records) ->
    case dialyzer_codeserver:lookup_mfa_contract(MFA, CodeServer) of
        error ->
            {{F, A}, {Range, Arg}};
        {ok, {_FileLine, Contract, _Xtra}} ->
            Sig = erl_types:t_fun(Arg, Range),
            case dialyzer_contracts:check_contract(Contract, Sig) of
                ok -> {{F, A}, {contract, Contract}};
                {range_warnings, _} -> {{F, A}, {contract, Contract}};
                {error, {overlapping_contract, []}} -> {{F, A}, {contract, Contract}}
            end
    end.

-spec get_functions(string(), #analysis{}) -> [any()].
get_functions(File, Analysis) ->
    Funcs = map__lookup(File, Analysis#analysis.func),
    Inc_Funcs = map__lookup(File, Analysis#analysis.inc_func),
    remove_module_info(Funcs) ++ normalize_incFuncs(Inc_Funcs).

-spec normalize_incFuncs([any()]) -> [any()].
normalize_incFuncs(Functions) ->
    [FunInfo || {_FileName, FunInfo} <- Functions].

-spec remove_module_info([func_info()]) -> [func_info()].
remove_module_info(FunInfoList) ->
    F = fun
        ({_, module_info, 0}) -> false;
        ({_, module_info, 1}) -> false;
        ({Line, F, A}) when is_integer(Line), is_atom(F), is_integer(A) -> true
    end,
    lists:filter(F, FunInfoList).

-spec get_type_spec(atom(), arity(), #info{}) -> string().
get_type_spec(F, A, Info) ->
    Type = get_type_info({F, A}, Info#info.types),
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

-spec get_type_info({any(), any()}, dict:dict()) -> {any(), any()}.
get_type_info(Func, Types) ->
    case map__lookup(Func, Types) of
        {contract, _Fun} = C -> C;
        {_RetType, _ArgType} = RA -> RA
    end.

-type inc_file_info() :: {file:filename(), func_info()}.
-record(tmpAcc, {
    file :: file:filename(),
    module :: atom(),
    funcAcc = [] :: [func_info()],
    incFuncAcc = [] :: [inc_file_info()],
    dialyzerObj = [] :: [{mfa(), {_, _}}]
}).

-spec collect_info(analysis()) -> analysis().
collect_info(Analysis) ->
    DialyzerPlt = get_dialyzer_plt(),
    NewPlt = dialyzer_plt:merge_plts([Analysis#analysis.trust_plt, DialyzerPlt]),
    NewAnalysis = lists:foldl(
        fun collect_one_file_info/2,
        Analysis#analysis{trust_plt = NewPlt},
        Analysis#analysis.files
    ),
    TmpCServer = NewAnalysis#analysis.codeserver,
    TmpCServer1 = dialyzer_utils:merge_types(TmpCServer, NewPlt),
    NewExpTypes = dialyzer_codeserver:get_temp_exported_types(TmpCServer),
    OldExpTypes = dialyzer_plt:get_exported_types(NewPlt),
    MergedExpTypes = sets:union(NewExpTypes, OldExpTypes),
    TmpCServer2 =
        dialyzer_codeserver:finalize_exported_types(MergedExpTypes, TmpCServer1),
    TmpCServer3 = dialyzer_utils:process_record_remote_types(TmpCServer2),
    NewCServer = dialyzer_contracts:process_contract_remote_types(TmpCServer3),
    NewAnalysis#analysis{codeserver = NewCServer}.

-spec collect_one_file_info(string(), #analysis{}) -> #analysis{}.
collect_one_file_info(File, Analysis) ->
    Macros = Analysis#analysis.macros,
    Includes = Analysis#analysis.includes,
    Options = dialyzer_utils:src_compiler_opts() ++ Includes ++ Macros,
    {ok, Core} = dialyzer_utils:get_core_from_src(File, Options),
    {ok, Records} = dialyzer_utils:get_record_and_type_info(Core),
    Mod = cerl:concrete(cerl:module_name(Core)),
    {ok, SpecInfo, CbInfo} = dialyzer_utils:get_spec_info(Mod, Core, Records),
    ExpTypes = get_exported_types_from_core(Core),
    analyze_core_tree(
        Core,
        Records,
        SpecInfo,
        CbInfo,
        ExpTypes,
        Analysis,
        File
    ).

-spec analyze_core_tree(any(), any(), any(), any(), sets:set(_), #analysis{}, string()) ->
    #analysis{}.
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
    Ex_Funcs = [{0, F, A} || {_, _, {F, A}} <- cerl:module_exports(Tree)],
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
    IncFuncMap = map__insert(
        {File, Acc#tmpAcc.incFuncAcc},
        Analysis#analysis.inc_func
    ),
    FMs = Analysis#analysis.fms ++ [{File, Module}],
    RecordMap = map__insert({File, Records}, Analysis#analysis.record),
    Analysis#analysis{
        fms = FMs,
        callgraph = CG,
        codeserver = CS6,
        ex_func = Exported_FuncMap,
        inc_func = IncFuncMap,
        record = RecordMap,
        func = FuncMap
    }.

-spec analyze_one_function({any(), any()}, #tmpAcc{}) -> #tmpAcc{}.
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
            %% Coming from original file
            true ->
                %% io:format("Added function ~tp\n", [{LineNo, F, A}]),
                {Acc#tmpAcc.funcAcc ++ [FuncInfo], Acc#tmpAcc.incFuncAcc};
            false ->
                %% Coming from other sourses, including:
                %%     -- .yrl (yecc-generated file)
                %%     -- yeccpre.hrl (yecc-generated file)
                %%     -- other cases
                {Acc#tmpAcc.funcAcc, Acc#tmpAcc.incFuncAcc ++ [{FileName, FuncInfo}]}
        end,
    Acc#tmpAcc{
        funcAcc = FuncAcc,
        incFuncAcc = IncFuncAcc,
        dialyzerObj = NewDialyzerObj
    }.

-spec get_line([line()]) -> 'none' | integer().
get_line([Line | _]) when is_integer(Line) -> Line;
get_line([_ | T]) -> get_line(T);
get_line([]) -> none.

-spec get_file([any()]) -> any().
get_file([_ | T]) -> get_file(T);
% should not happen
get_file([]) -> "no_file".

-spec get_dialyzer_plt() -> plt().
get_dialyzer_plt() ->
    PltFile =
        case els_config:get(plt_path) of
            undefined ->
                default_plt_file();
            PltPath ->
                PltPath
        end,
    plt_from_file(PltFile).

%% Exported Types

-spec get_exported_types_from_core(any()) -> sets:set().
get_exported_types_from_core(Core) ->
    Attrs = cerl:module_attrs(Core),
    ExpTypes1 = [
        cerl:concrete(L2)
     || {L1, L2} <- Attrs,
        cerl:is_literal(L1),
        cerl:is_literal(L2),
        cerl:concrete(L1) =:= 'export_type'
    ],
    ExpTypes2 = lists:flatten(ExpTypes1),
    M = cerl:atom_val(cerl:module_name(Core)),
    sets:from_list([{M, F, A} || {F, A} <- ExpTypes2]).

%%--------------------------------------------------------------------
%% Handle messages.
%%--------------------------------------------------------------------

-spec rcv_ext_types() -> [any()].
rcv_ext_types() ->
    Self = self(),
    Self ! {Self, done},
    rcv_ext_types(Self, []).

-spec rcv_ext_types(pid(), [any()]) -> [any()].
rcv_ext_types(Self, ExtTypes) ->
    receive
        {Self, ext_types, ExtType} ->
            rcv_ext_types(Self, [ExtType | ExtTypes]);
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
    try
        dict:fetch(Key, Map)
    catch
        error:_ -> none
    end.

-spec map__from_list([{fa(), term()}]) -> map_dict().
map__from_list(List) ->
    dict:from_list(List).

-dialyzer({nowarn_function, default_plt_file/0}).

-spec default_plt_file() -> file:filename().
default_plt_file() ->
    %% OTP 26+ uses dialyzer_iplt
    case erlang:function_exported(dialyzer_iplt, get_default_iplt_filename, 0) of
        true ->
            dialyzer_iplt:get_default_iplt_filename();
        false ->
            dialyzer_plt:get_default_plt()
    end.

-dialyzer({nowarn_function, plt_from_file/1}).

-spec plt_from_file(file:filename()) -> dialyzer_plt:plt().
plt_from_file(PltFile) ->
    %% OTP 26+ uses dialyzer_iplt
    case erlang:function_exported(dialyzer_iplt, from_file, 1) of
        true ->
            dialyzer_iplt:from_file(PltFile);
        false ->
            dialyzer_plt:from_file(PltFile)
    end.
