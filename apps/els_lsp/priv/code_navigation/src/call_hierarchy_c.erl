-module(call_hierarchy_c).

-export([ function_a/1
        , function_c/0
        ]).

function_a(0) ->
  ok;
function_a(N) ->
  function_b(),
  function_b(),
  function_a(N-1).

function_b() ->
  ok.

function_c() ->
  {}.
