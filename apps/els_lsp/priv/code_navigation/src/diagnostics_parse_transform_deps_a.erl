-module(diagnostics_parse_transform_deps_a).

-compile({parse_transform, diagnostics_parse_transform_deps_b}).

unused() ->
  ok.
