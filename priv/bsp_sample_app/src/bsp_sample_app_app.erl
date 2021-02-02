%%%-------------------------------------------------------------------
%% @doc bsp_sample_app public API
%% @end
%%%-------------------------------------------------------------------

-module(bsp_sample_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    bsp_sample_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
