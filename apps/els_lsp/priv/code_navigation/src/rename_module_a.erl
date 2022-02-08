-module(rename_module_a).

-export([ function_a/0
        , function_b/0
        ]).
-export_type([type_a/0]).

-type type_a() :: any().

-callback function_a() -> type_a().

function_a() ->
  a.

function_b() ->
  b.
