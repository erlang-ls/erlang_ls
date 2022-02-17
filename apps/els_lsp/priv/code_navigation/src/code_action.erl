-module(code_action_oops).

-export([function_a/0]).

function_a() ->
  A = 123,
  function_b().

function_b() ->
  ok.

function_c() ->
  Foo = 1,
  Bar = 2,
  Foo + Barf.
