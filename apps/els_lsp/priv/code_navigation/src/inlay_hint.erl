-module(inlay_hint).
-export([test/0]).

-record(foo, {}).

test() ->
    a(1, 2),
    b(1, 2),
    c(1),
    d(1, 2),
    lists:append([], []).

a(Hej, Hoj) ->
    Hej + Hoj.

b(x, y) ->
    0;
b(A, _B) ->
    A.

c(#foo{}) ->
    ok.

d([1,2,3] = Foo,
  Bar = #{hej := 123}) ->
    ok.
