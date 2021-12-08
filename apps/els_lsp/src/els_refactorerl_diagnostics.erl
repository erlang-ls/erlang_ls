%%==============================================================================
%% Unsecure Calls by Referl
%%==============================================================================
-module(els_refactorerl_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").
-define(MAX_RECURSION_DEPTH, 10).

%%==============================================================================
%% Types
%%==============================================================================
-type refactorerl_diagnostic_id() :: {atom(), [char()], [char()], [char()]}.
-type refactorerl_query() :: [char()].

%%==============================================================================
%% Callback Functions
%%==============================================================================



-spec make_query(refactorerl_diagnostic_id(), module()) -> refactorerl_query().
make_query({_, _, Before, After}, Module) ->
  ModuleStr = atom_to_list(Module),
  Before ++ ModuleStr ++ After.


-spec run_query(module(), refactorerl_diagnostic_id()) ->
                                          ([els_diagnostics:diagnostic()]).
run_query(Module, DiagnosticId) ->
  {_, Message, _, _} = DiagnosticId,
  ReferlResult = els_refactorerl_utils:query(make_query(DiagnosticId, Module)),
  Pois = els_refactorerl_utils:convert_to_poi(ReferlResult),
  [make_diagnostic(Poi, Message) || Poi <- Pois].

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  run(Uri, 0).


-spec run(uri(), number()) -> [els_diagnostics:diagnostic()].
run(Uri, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH ->
  case filename:extension(Uri) of
    <<".erl">> ->
      case els_refactorerl_utils:referl_node() of
        disabled ->
          [];
        _ ->
          case add(Uri) of
            busy ->
              timer:sleep(1000),
              run(Uri, RecursionDepth + 1);
            disabled ->
              [];
            _ ->
              FileName = filename:basename(binary_to_list(els_uri:path(Uri))),
              Module = list_to_atom(filename:rootname(FileName)),
              Diagnostics = refactorerl_diagnostics(),
              lists:concat([run_query(Module, DiagId) || DiagId <- Diagnostics])
          end
      end;
    _ ->
      []
  end;

run(_, RecursionDepth) when RecursionDepth >= ?MAX_RECURSION_DEPTH ->
  Param = #{ type => ?MESSAGE_TYPE_ERROR,
             message => <<"Cannot add module to RefactorErl!">> },
  els_server:send_notification(<<"window/showMessage">>, Param),
  [].

-spec source() -> binary().
source() ->
  <<"RefactorErl Diagnostics">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec make_diagnostic(poi(), [char()]) -> els_diagnostics:diagnostic().
make_diagnostic(#{ data := PoiData, range := PoiRange}, DiagMessage) ->
    Range = els_protocol:range(PoiRange),
    Message = list_to_binary(DiagMessage ++ " " ++ PoiData),
    Severity = ?DIAGNOSTIC_WARNING,
    Source = source(),
    els_diagnostics:make_diagnostic(Range, Message, Severity, Source).

-spec refactorerl_diagnostics() -> [refactorerl_diagnostic_id()].
refactorerl_diagnostics() ->
  [ {unused_macros, "Security Issue",  "mods[name=", "].funs.unsecure_calls"}
  ,   {unsecure_calls
      , "Unused Macros:"
      , "mods[name="
      , "].macros[not .references]" }
  ].

-spec add(any()) -> atom().
add(Uri) ->
  Param = [binary_to_list(els_uri:path(Uri))],
  rpc:call(node(), ri, add, Param, els_refactorerl_utils:maxtimeout()).