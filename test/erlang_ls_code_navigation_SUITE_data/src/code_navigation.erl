-module(code_navigation).

-export([ function_a/0 ]).

-record(record_a, {field_a, field_b}).

-define(MACRO_A, macro_a).

function_a() ->
  function_b(),
  #record_a{}.

function_b() ->
  ?MACRO_A.
