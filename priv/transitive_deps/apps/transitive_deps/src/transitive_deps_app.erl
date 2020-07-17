%%%-------------------------------------------------------------------
%% @doc transitive_deps public API
%% @end
%%%-------------------------------------------------------------------

-module(transitive_deps_app).

-behaviour(application).
-behaviour(behaviour_b).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    transitive_deps_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
