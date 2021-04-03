-module(rename_function_import).
-export([function_a/0]).
-import(rename_function, [foo/1]).

-spec function_a() -> any().
function_a() ->
  foo(1),
  rename_function:foo(2).
