-module(rename_function_import).
-export([function_a/0]).
-import(rename_function, [foo/0]).

-spec function_a() -> any().
function_a() ->
  foo(),
  rename_function:foo().
