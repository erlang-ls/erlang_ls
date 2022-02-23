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

%%==============================================================================
%% Types
%%==============================================================================
-type refactorerl_diagnostic_description() :: {string(), string()}.
%-type refactorerl_query() :: [char()].

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  false.

  -spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      case els_refactorerl_utils:referl_node() of
        {error, _} ->
          [];
        {ok, _} ->
          case els_refactorerl_utils:add(Uri) of
            error ->
              [];
            ok ->
              Module = els_uri:module(Uri),
              Diags = enabled_diagnostics(),
              Results = els_refactorerl_utils:run_diagnostics(Diags, Module),
              make_diagnostics(Results)
          end
      end;
    _ ->
      []
  end.

-spec source() -> binary().
source() ->
  els_refactorerl_utils:source_name().

%%==============================================================================
%% Internal Functions
%%==============================================================================




%TODO: add default diagnostics

-spec diagnostics_config() -> list().
diagnostics_config() ->
  case els_config:get(refactorerl) of
    #{"diagnostics" := List} ->
      List;
    _ ->
      []
  end.

-spec enabled_diagnostics() -> [refactorerl_diagnostic_description()].
enabled_diagnostics() ->
  %%Diagnostics = refactorerl_diagnostics(),
  EnabledDiagnostics = diagnostics_config(),
  %%enabled_diagnostics(EnabledDiagnostics, Diagnostics).
  EnabledDiagnostics.





-spec make_diagnostics(any()) -> any().
make_diagnostics([{Range, Message} | Tail]) ->
  Severity = ?DIAGNOSTIC_WARNING,
  Source = source(),
  Diag = els_diagnostics:make_diagnostic(Range, Message, Severity, Source),
  [ Diag | make_diagnostics(Tail) ];

make_diagnostics([]) ->
  [].
