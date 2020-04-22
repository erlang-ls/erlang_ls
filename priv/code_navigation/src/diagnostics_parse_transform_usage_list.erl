-module(diagnostics_parse_transform_usage_list).

-export([ main/1 ]).

-compile([{parse_transform, diagnostics_parse_transform}, debug_info]).

main(Args) ->
  ok.
