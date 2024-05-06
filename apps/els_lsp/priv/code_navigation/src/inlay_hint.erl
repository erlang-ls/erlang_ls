-module(inlay_hint).
-export([test/0]).

-record(foo, {}).

test() ->
    a(1, 2),
    b(1, 2),
    c(1),
    d(1, 2),
    e(1, 2),
    f(1, 2),
    g(1, 2),
    lists:append([], []).

a(A1, A2) ->
    A1 + A2.

b(x, y) ->
    0;
b(B1, _B2) ->
    B1.

c(#foo{}) ->
    ok.

d([1,2,3] = D1,
  D2 = #{hej := 123}) ->
    ok.

-spec e(E1 :: any(), E2 :: any()) -> ok.
e(_, _) ->
    ok.

-spec f(F1, F2) -> ok when F1 :: any(), F2 :: any().
f(_, _) ->
    ok.

-spec g(G1, any()) -> ok when G1 :: any().
g(_, G2) ->
    ok.
