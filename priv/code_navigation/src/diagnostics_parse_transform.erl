-module(diagnostics_parse_transform).

-export([ parse_transform/2 ]).

parse_transform(AST, _Opts) ->
  AST.
