%%%=============================================================================
%%% @doc Erlang LS EDoc Plugin
%%% @end
%%%=============================================================================
-module(els_plugin_edoc).

-behaviour(els_plugin).
-export([ name/0
        , provides/0
        , init/0
        , is_enabled/0
        , on_diagnostics_run/1
        ]).

name() -> <<"Edoc">>.

provides() ->
  #{ diagnostics => true }.

init() ->
  ok.

is_enabled() ->
  %% TODO: Be able to configure this via config
  true.

on_diagnostics_run(Uri) ->
  els_edoc_diagnostics:run(Uri).
