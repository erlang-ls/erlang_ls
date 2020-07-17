-module(behaviour_b).

-export([bc_cb/0]).

-behaviour(behaviour_c).

-callback ba_cb() ->
    term().

-spec bc_cb() -> ok.
bc_cb() ->
    ok.
