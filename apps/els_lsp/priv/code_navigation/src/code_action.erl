%% Important: this file feeds the tests in the els_code_action_SUITE.erl.
%% Please only add new cases from bottom, otherwise it might break those tests.
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

-define(TIMEOUT, 200).
