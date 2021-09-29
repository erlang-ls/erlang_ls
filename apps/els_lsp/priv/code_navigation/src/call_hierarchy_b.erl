-module(call_hierarchy_b).

-export([ function_a/1
        , function_c/0
        ]).

function_a(0) ->
  ok;
function_a(N) ->
  function_b(),
  call_hierarchy_a:function_a(31),
  function_b(),
  function_a(N-1).

function_b() ->
  call_hierarchy_a:function_b().

function_c() ->
  {}.
