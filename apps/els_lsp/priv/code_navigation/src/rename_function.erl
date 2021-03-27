-module(rename_function).
-export([foo/0]).

-spec foo() -> any().
foo() ->
  bar().

-spec foo(any()) -> any().
foo(_) ->
  foo().

bar() ->
  ok = rename_function:foo(),
  F = fun foo/0,
  ok = F(),
  F2 = fun rename_function:foo/0,
  ok = F2(),
  F3 = fun foo/1,
  foo(F3),
  foo().
