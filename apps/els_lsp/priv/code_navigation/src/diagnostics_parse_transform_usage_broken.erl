-module(diagnostics_parse_transform_usage_broken).

-export([ main/1 ]).

-compile({parse_transform, diagnostics_parse_transform_broken}).

main(Args) ->
  ok.
