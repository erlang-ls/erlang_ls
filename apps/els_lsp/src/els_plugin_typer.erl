%%%=============================================================================
%%% @doc Erlang LS EDoc Plugin
%%% @end
%%%=============================================================================
-module(els_plugin_typer).

-behaviour(els_plugin).
-export([ name/0
        , provides/0
        , init/0
        , is_enabled/0
        , lenses/1
        ]).

name() -> <<"Edoc">>.

provides() ->
  #{ lenses => true }.

init() ->
  ok.

is_enabled() ->
  true.

%% TODO: Should we pass a Uri instead?
lenses(Document) ->
  els_code_lens_suggest_spec:
