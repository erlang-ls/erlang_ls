%%%-------------------------------------------------------------------
%% @doc rebar3_release public API
%% @end
%%%-------------------------------------------------------------------

-module(rebar3_release_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rebar3_release_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
