-module(diagnostics_parse_transform_deps_b).

-export([ parse_transform/2 ]).

parse_transform(Forms, _Opts) ->
  diagnostics_parse_transform_deps_c:dummy(),
  Forms.
