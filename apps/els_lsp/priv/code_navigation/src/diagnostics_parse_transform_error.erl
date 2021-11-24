-module(diagnostics_parse_transform_error).

-compile({parse_transform, my_parse_transform}).

-export([ function_a/0 ]).

function_a() ->
  ok.
