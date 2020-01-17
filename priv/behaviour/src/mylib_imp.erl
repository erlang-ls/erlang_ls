-module(mylib_imp).

-behaviour(mylib_behaviour).

-export([init/1, foo/2]).

init(A) ->
    ok.

foo(_A,_B) ->
    ok.
