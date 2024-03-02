-module(completion_functionclause).

test_a(ArgA, ArgB) when
    (ArgA == 123);
    (ArgB == 321) ->
  test_a.

test_b(ArgA, ArgB, ArgC) ->
    test_b;
test_b(ArgA, ArgB, ArgC) ->
    ArgA + ArgB + ArgC;
