%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_compiler_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-behaviour(els_diagnostics).
-export([ is_default/0
        , run/1
        , source/0
        , on_complete/2
        ]).

%% identity function for our own diagnostics
-export([ format_error/1 ]).

-export([ inclusion_range/2
        , inclusion_range/3
        ]).

-export([ include_options/0
        , macro_options/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type compiler_info()  :: {erl_anno:anno() | 'none', module(), any()}.
-type compiler_msg()   :: {file:filename(), [compiler_info()]}.
-type macro_config()   :: #{string() => string()}.
-type macro_option()   :: {'d', atom()} | {'d', atom(), any()}.
-type include_option() :: {'i', string()}.

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      compile(Uri);
    <<".hrl">> ->
      %% It does not make sense to 'compile' header files in isolation
      %% (e.g. using the compile:forms/1 function). That would in fact
      %% produce a big number of false positive errors and warnings,
      %% including 'record not used' or 'module attribute not
      %% specified'. An alternative could be to use a 'fake' module
      %% that simply includes the file, but that feels a bit too
      %% hackish. As a compromise, we decided to parse the include
      %% file, since that allows us to identify most of the common
      %% errors in header files.
      parse(Uri);
    <<".escript">> ->
      parse_escript(Uri);
    _Ext ->
      ?LOG_DEBUG("Skipping diagnostics due to extension [uri=~p]", [Uri]),
      []
  end.

-spec source() -> binary().
source() ->
  <<"Compiler">>.

-spec on_complete(uri(), [els_diagnostics:diagnostic()]) -> ok.
on_complete(Uri, Diagnostics) ->
  maybe_compile_and_load(Uri, Diagnostics).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec compile(uri()) -> [els_diagnostics:diagnostic()].
compile(Uri) ->
  Dependencies = els_diagnostics_utils:dependencies(Uri),
  Path = els_utils:to_list(els_uri:path(Uri)),
  case compile_file(Path, Dependencies) of
    {{ok, _, WS}, Diagnostics} ->
      Diagnostics ++
        diagnostics(Path, WS, ?DIAGNOSTIC_WARNING);
    {{error, ES, WS}, Diagnostics} ->
      Diagnostics ++
        diagnostics(Path, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(Path, ES, ?DIAGNOSTIC_ERROR)
  end.

-spec parse(uri()) -> [els_diagnostics:diagnostic()].
parse(Uri) ->
  FileName = els_utils:to_list(els_uri:path(Uri)),
  Document = case els_dt_document:lookup(Uri) of
                 {ok, [DocItem]} ->
                     DocItem;
                 _ ->
                     undefined
             end,
  {ok, Epp} = epp:open([ {name, FileName}
                       , {includes, els_config:get(include_paths)}
                       ]),
  Res = [epp_diagnostic(Document, Anno, Module, Desc)
         || {error, {Anno, Module, Desc}} <- epp:parse_file(Epp)],
  epp:close(Epp),
  Res.

%% Possible cases to handle
%% ,{error,{19,erl_parse,["syntax error before: ","'-'"]}}
%% ,{error,{1,epp,{error,1,{undefined,'MODULE',none}}}}
%% ,{error,{3,epp,{error,"including nonexistent_macro.hrl is not allowed"}}}
%% ,{error,{3,epp,{include,file,"yaws.hrl"}}}
-spec epp_diagnostic(els_dt_document:item(),
                     erl_anno:anno(), module(), any()) ->
        els_diagnostics:diagnostic().
epp_diagnostic(Document, Anno, epp, {error, Anno, Reason}) ->
    %% Workaround for https://bugs.erlang.org/browse/ERL-1310
    epp_diagnostic(Document, Anno, epp, Reason);
epp_diagnostic(Document, Anno, Module, Desc) ->
    diagnostic(range(Document, Anno), Module, Desc, ?DIAGNOSTIC_ERROR).

-spec parse_escript(uri()) -> [els_diagnostics:diagnostic()].
parse_escript(Uri) ->
  FileName = els_utils:to_list(els_uri:path(Uri)),
  case els_escript:extract(FileName) of
    {ok, WS} ->
      diagnostics(FileName, WS, ?DIAGNOSTIC_WARNING);
    {error, ES, WS} ->
      diagnostics(FileName, WS, ?DIAGNOSTIC_WARNING) ++
        diagnostics(FileName, ES, ?DIAGNOSTIC_ERROR)
  end.

%% @doc Convert compiler messages into diagnostics
%%
%% Convert a list of compiler messages of a given severity (warning,
%% error) into a list of diagnostic data structures, as expected by
%% the LSP protocol.
%% Compiler messages related to included files are grouped together
%% and they are presented to the user by highlighting the line where
%% the file inclusion happens.
-spec diagnostics(list(), [compiler_msg()], els_diagnostics:severity()) ->
        [els_diagnostics:diagnostic()].
diagnostics(Path, List, Severity) ->
  Uri = els_uri:uri(els_utils:to_binary(Path)),
  case els_utils:lookup_document(Uri) of
    {ok, Document} ->
      lists:flatten([[ diagnostic( Path
                                 , MessagePath
                                 , range(Document, Anno)
                                 , Document
                                 , Module
                                 , Desc
                                 , Severity)
                       || {Anno, Module, Desc} <- Info]
                     || {MessagePath, Info} <- List]);
    {error, _Error} ->
      []
  end.

-spec diagnostic( string()
                , string()
                , poi_range()
                , els_dt_document:item()
                , module()
                , string()
                , integer()) -> els_diagnostics:diagnostic().
diagnostic(Path, Path, Range, _Document, Module, Desc, Severity) ->
  %% The compiler message is related to the same .erl file, so
  %% preserve the location information.
  diagnostic(Range, Module, Desc, Severity);
diagnostic(_Path, MessagePath, Range, Document, Module, Desc0, Severity) ->
  #{from := {Line, _}} = Range,
  InclusionRange = inclusion_range(MessagePath, Document),
  %% The compiler message is related to an included file. Replace the
  %% original location with the location of the file inclusion.
  %% And re-route the format_error call to this module as a no-op
  Desc1 = Module:format_error(Desc0),
  Desc = io_lib:format("Issue in included file (~p): ~s", [Line, Desc1]),
  diagnostic(InclusionRange, ?MODULE, Desc, Severity).

-spec diagnostic(poi_range(), module(), string(), integer()) ->
        els_diagnostics:diagnostic().
diagnostic(Range, Module, Desc, Severity) ->
  Message0 = lists:flatten(Module:format_error(Desc)),
  Message  = els_utils:to_binary(Message0),
  #{ range    => els_protocol:range(Range)
   , message  => Message
   , severity => Severity
   , source   => source()
   }.

%% @doc NOP function for the call to 'Module:format_error/1' in diagnostic/4
%% above.
-spec format_error(string()) -> [string()].
format_error(Str) ->
  Str.

-spec range(els_dt_document:item() | undefined,
            erl_anno:anno() | none) -> poi_range().
range(Document, none) ->
    range(Document, erl_anno:new(1));
range(Document, Anno) ->
    true = erl_anno:is_anno(Anno),
    Line = erl_anno:line(Anno),
    case erl_anno:column(Anno) of
        Col when Document =:= undefined; Col =:= undefined ->
            #{from => {Line, 1}, to => {Line + 1, 1}};
        Col ->
            Ranges = els_dt_document:get_element_at_pos(Document, Line, Col),

            %% * If we find no pois that we just return the original line
            %% * If we find a poi that start on the line and col as the anno
            %%   we are looking for we that that one.
            %% * We take the "first" poi if we find some, but none come from
            %%   the correct line and number.

            case lists:search(
                   fun(#{ range := #{ from := {FromLine, FromCol} } }) ->
                           FromLine =:= Line andalso FromCol =:= Col
                   end, Ranges) of
                {value, #{ range := Range } } ->
                    Range;
                false when Ranges =:= [] ->
                    #{ from => {Line, 1}, to => {Line + 1, 1} };
                false ->
                    maps:get(range, hd(Ranges))
            end
    end.

%% @doc Find the inclusion range for a header file.
%%
%%      Given the path of e .hrl path, find its inclusion range within
%%      a given document.
-spec inclusion_range(string(), els_dt_document:item()) -> poi_range().
inclusion_range(IncludePath, Document) ->
  case
    inclusion_range(IncludePath, Document, include) ++
    inclusion_range(IncludePath, Document, include_lib) ++
    inclusion_range(IncludePath, Document, behaviour) ++
    inclusion_range(IncludePath, Document, parse_transform)
  of
    [Range|_] -> Range;
    _ -> range(undefined, none)
  end.

-spec inclusion_range( string()
                     , els_dt_document:item()
                     , include | include_lib | behaviour | parse_transform)
                     -> [poi_range()].
inclusion_range(IncludePath, Document, include) ->
  POIs       = els_dt_document:pois(Document, [include]),
  IncludeId  = els_utils:include_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId];
inclusion_range(IncludePath, Document, include_lib) ->
  POIs       = els_dt_document:pois(Document, [include_lib]),
  IncludeId  = els_utils:include_lib_id(IncludePath),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= IncludeId];
inclusion_range(IncludePath, Document, behaviour) ->
  POIs        = els_dt_document:pois(Document, [behaviour]),
  BehaviourId = els_uri:module(els_uri:uri(els_utils:to_binary(IncludePath))),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= BehaviourId];
inclusion_range(IncludePath, Document, parse_transform) ->
  POIs       = els_dt_document:pois(Document, [parse_transform]),
  ParseTransformId
    = els_uri:module(els_uri:uri(els_utils:to_binary(IncludePath))),
  [Range || #{id := Id, range := Range} <- POIs, Id =:= ParseTransformId].

-spec macro_options() -> [macro_option()].
macro_options() ->
  Macros = els_config:get(macros),
  [macro_option(M) || M <- Macros].

-spec macro_option(macro_config()) -> macro_option().
macro_option(#{"name" := Name, "value" := Value}) ->
  {'d', list_to_atom(Name), els_utils:macro_string_to_term(Value)};
macro_option(#{"name" := Name}) ->
  {'d', list_to_atom(Name), true}.

-spec include_options() -> [include_option()].
include_options() ->
  Paths = els_config:get(include_paths),
  [ {i, Path} || Path <- Paths ].

-spec diagnostics_options() -> [any()].
diagnostics_options() ->
  [basic_validation|diagnostics_options_bare()].

-spec diagnostics_options_load_code() -> [any()].
diagnostics_options_load_code() ->
  [binary|diagnostics_options_bare()].

-spec diagnostics_options_bare() -> [any()].
diagnostics_options_bare() ->
  lists:append([ macro_options()
               , include_options()
               , [ return_warnings
                 , return_errors
                 ]]).

-spec compile_file(string(), [atom()]) ->
        {{ok | error, [compiler_msg()], [compiler_msg()]}
        , [els_diagnostics:diagnostic()]}.
compile_file(Path, Dependencies) ->
  %% Load dependencies required for the compilation
  Olds = [load_dependency(Dependency, Path)
          || Dependency <- Dependencies
               , not code:is_sticky(Dependency) ],
  Res = compile:file(Path, diagnostics_options()),
  %% Restore things after compilation
  [code:load_binary(Dependency, Filename, Binary)
   || {{Dependency, Binary, Filename}, _} <- Olds],
  Diagnostics = lists:flatten([ Diags || {_, Diags} <- Olds ]),
  {Res, Diagnostics}.

%% @doc Load a dependency, return the old version of the code (if any),
%% so it can be restored.
-spec load_dependency(atom(), string()) ->
        {{atom(), binary(), file:filename()}, [els_diagnostics:diagnostic()]}
          | error.
load_dependency(Module, IncludingPath) ->
  Old = code:get_object_code(Module),
  Diagnostics =
    case els_utils:find_module(Module) of
      {ok, Uri} ->
        Path = els_utils:to_list(els_uri:path(Uri)),
        Opts = compile_options(Module),
        case compile:file(Path, diagnostics_options_load_code() ++ Opts) of
          {ok, [], []} ->
            [];
          {ok, Module, Binary} ->
            code:load_binary(Module, atom_to_list(Module), Binary),
            [];
          {ok, Module, Binary, WS} ->
            code:load_binary(Module, atom_to_list(Module), Binary),
            diagnostics(IncludingPath, WS, ?DIAGNOSTIC_WARNING);
          {error, ES, WS} ->
            diagnostics(IncludingPath, WS, ?DIAGNOSTIC_WARNING) ++
              diagnostics(IncludingPath, ES, ?DIAGNOSTIC_ERROR)
        end;
      {error, Error} ->
        ?LOG_WARNING( "Error finding dependency [module=~p] [error=~p]"
                    , [Module, Error]),
        []
    end,
  {Old, Diagnostics}.

-spec maybe_compile_and_load(uri(), [els_diagnostics:diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := NodeStr} ->
      Node = els_utils:compose_node_name(NodeStr,
                                         els_config_runtime:get_name_type()),
      Module = els_uri:module(Uri),
      case rpc:call(Node, code, is_sticky, [Module]) of
        true -> ok;
        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
      end;
    disabled ->
      ok
  end;
maybe_compile_and_load(_Uri, _CDiagnostics) ->
  ok.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
  Msg = io_lib:format("code_reload success for: ~s", [Module]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => els_utils:to_binary(Msg)
                                });
handle_rpc_result(Err, Module) ->
  ?LOG_INFO("[code_reload] code_reload using c:c/1 crashed with: ~p",
            [Err]),
  Msg = io_lib:format("code_reload swap crashed for: ~s with: ~w",
                      [Module, Err]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_ERROR,
                                  message => els_utils:to_binary(Msg)
                                }).

%% @doc Return the compile options from the compile_info chunk
-spec compile_options(atom()) -> [any()].
compile_options(Module) ->
  case code:which(Module) of
    non_existing ->
      ?LOG_INFO("Could not find compile options. [module=~p]", [Module]),
      [];
    Beam ->
      case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {_, Chunks}} ->
          Info = proplists:get_value(compile_info, Chunks),
          proplists:get_value(options, Info, []);
        Error ->
          ?LOG_INFO( "Error extracting compile_info. [module=~p] [error=~p]"
                   , [Module, Error]),
          []
      end
  end.
