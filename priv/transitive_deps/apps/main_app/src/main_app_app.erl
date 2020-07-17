%%%-------------------------------------------------------------------
%% @doc transitive_deps public API
%% @end
%%%-------------------------------------------------------------------

-module(main_app_app).

-behaviour(application).

-behaviour(behaviour_b).

-export([start/2, stop/1]).

-export([ba_cb/0]).

start(_StartType, _StartArgs) ->
    main_app_sup:start_link().

stop(_State) ->
    ok.

ba_cb() -> ok.

%% internal functions
