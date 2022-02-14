-module(rename_module_b).

-behaviour(rename_module_a).
-import(rename_module_a, [function_b/0]).

-export([function_a/0]).

-type type_a() :: rename_module_a:type_a().

-spec function_a() -> type_a().
function_a() ->
  rename_module_a:function_a(),
  F = fun rename_module_a:function_a/0,
  F().
