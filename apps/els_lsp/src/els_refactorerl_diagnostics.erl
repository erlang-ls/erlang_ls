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
-type refactorerl_diagnostic_description() :: {[char()], [char()]}.
-type refactorerl_query() :: [char()].

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
              FileName = filename:basename(binary_to_list(els_uri:path(Uri))),
              Module = list_to_atom(filename:rootname(FileName)),
              Diagnostics = enabled_diagnostics(),
              lists:concat([run_query(Module, DiagDesc) || DiagDesc <- Diagnostics])
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


%%@doc
%% Returns the available diagnostics of RefactorErl.
-spec refactorerl_diagnostics() ->
                       #{ atom() => refactorerl_diagnostic_description() }.
refactorerl_diagnostics() ->  %//? Kérdés: atom szebb kulcsként vagy stringként, mert config stringként adja vissza, konvertáljuk?
  #{
    % Unused Macros
    "unused_macros" =>
      {"Unused Macros:", "].macros[not .references]"},

    % Detecting vulnerabilities
    "unsecure_calls" =>  
      {"Security Issue", "].funs.unsecure_calls"}
  }
.

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
  Diagnostics = refactorerl_diagnostics(),
  EnabledDiagnostics = diagnostics_config(),
  enabled_diagnostics(EnabledDiagnostics, Diagnostics).

 
-spec enabled_diagnostics(list(), map()) -> [refactorerl_diagnostic_description()].
enabled_diagnostics([ ConfigDiag | RemainingDiags ], Diagnostics) ->
  case maps:find(ConfigDiag, Diagnostics) of
    {ok, Value} ->
      [Value] ++ enabled_diagnostics(RemainingDiags, Diagnostics);
    error ->
      enabled_diagnostics(RemainingDiags, Diagnostics)
  end;

enabled_diagnostics([], _) ->
  [].

%%@doc
%% Creates a RefactorErl query from a diagnostic identifier and a module name
-spec make_query(refactorerl_diagnostic_description(), module()) -> refactorerl_query().
make_query({_, After}, Module) ->
  ModuleStr = atom_to_list(Module),
  "mods[name=" ++ ModuleStr ++ After.


-spec run_query(module(), refactorerl_diagnostic_description()) ->
                                          ([els_diagnostics:diagnostic()]).
run_query(Module, {Message, _} = DiagnosticDesc) ->
  ReferlResult = els_refactorerl_utils:query(make_query(DiagnosticDesc, Module)),
  els_refactorerl_utils:make_diagnostics(ReferlResult, Message).
