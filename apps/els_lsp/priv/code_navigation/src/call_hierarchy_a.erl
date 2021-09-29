-module(call_hierarchy_a).

-export([ function_a/1
        , function_c/0
        ]).

function_a(0) ->
  ok;
function_a(N) ->
  case N of
    42 ->
      function_b()
  end,
  call_hierarchy_b:function_a(2),
  function_b(),
  function_a(N-1).

function_b() ->
  ok.

function_c() ->
  {}.
