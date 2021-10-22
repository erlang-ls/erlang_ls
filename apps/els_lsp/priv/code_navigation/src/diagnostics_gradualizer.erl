-module(diagnostics_gradualizer).

-export([g/1]).

-spec f(integer()) -> integer().
f(N) ->
    N.

-spec g(boolean()) -> boolean().
g(N) ->
    f(N).
