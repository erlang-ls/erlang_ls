-module(diagnostics_parse_transform_usage).

-export([ main/1 ]).

-compile({parse_transform, diagnostics_parse_transform}).

main(Args) ->
  ok.
