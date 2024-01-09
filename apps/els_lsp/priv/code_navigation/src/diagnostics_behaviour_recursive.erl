-module(diagnostics_behaviour_recursive).
-behaviour(diagnostics_behaviour).

-export([one/0]).
-export([two/0]).

-callback three() -> ok.

one() ->
    ok.
two() ->
    ok.
