%%==============================================================================
%% RefactorErl Diagnostics
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

-spec is_default() -> boolean().
is_default() ->
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  run(Uri, 0).


-spec run(uri(), number()) -> [els_diagnostics:diagnostic()].
run(Uri, RecursionDepth) when RecursionDepth < ?MAX_RECURSION_DEPTH ->
  case filename:extension(Uri) of
    <<".erl">> ->
      case els_refactorerl_utils:referl_node() of
        {error, _} ->
          [];
        {ok, _} ->
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
  Msg = "Cannot add module to RefactorErl!",
  els_refactorerl_utils:notification(Msg, ?MESSAGE_TYPE_ERROR),
  [].

-spec source() -> binary().
source() ->
  els_refactorerl_utils:source_name().

%%==============================================================================
%% Internal Functions
%%==============================================================================


%%@doc
%% Returns the available diagnostics of RefactorErl.
-spec refactorerl_diagnostics() -> [refactorerl_diagnostic_id()].
refactorerl_diagnostics() -> 
  [ {unused_calls, "Security Issue",  "mods[name=", "].funs.unsecure_calls"}
  ,   {unsecure_macros
      , "Unused Macros:"
      , "mods[name="
      , "].macros[not .references]" }
  ].

  %%@doc
  %% Adds a module to the RefactorErl node.
-spec add(any()) -> atom().
add(Uri) ->
  els_refactorerl_utils:add(Uri).

%%@doc
%% Creates a RefactorErl query from a diagnostic identifier and a module name
-spec make_query(refactorerl_diagnostic_id(), module()) -> refactorerl_query().
make_query({_, _, Before, After}, Module) ->
  ModuleStr = atom_to_list(Module),
  Before ++ ModuleStr ++ After.


-spec run_query(module(), refactorerl_diagnostic_id()) ->
                                          ([els_diagnostics:diagnostic()]).
run_query(Module, DiagnosticId) ->
  {_, Message, _, _} = DiagnosticId,
  ReferlResult = els_refactorerl_utils:query(make_query(DiagnosticId, Module)),
  els_refactorerl_utils:make_diagnostics(ReferlResult, Message).
