-module(diagnostics).

-include("diagnostics.hrl").
-include("broken_diagnostics.hrl").

-spec main(defined_type()) -> undefined_type().
main(X) ->
  X.
