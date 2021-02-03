-module(diagnostics_parse_transform_broken).

-export([ parse_transform/2 ]).

parse_transform(AST, _Opts) ->
  AST.

%% Add a bug to make sure the module using this parse transform generates a
%% diagnostic for it.
bug
