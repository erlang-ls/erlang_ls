-module(diagnostics).

-include("diagnostics.hrl").
-include("broken_diagnostics.hrl").

%% utf-8: （・ｗ・）
-spec main(defined_type()) -> undefined_type().
main(X) ->
  X.
