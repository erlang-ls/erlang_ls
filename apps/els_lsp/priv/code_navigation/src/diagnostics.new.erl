-module(diagnostics).
%% Changed diagnostics.erl, to test diff generation

%% Added utf-8: （・ω・）
-include("diagnostics.hrl").
-include("broken_diagnostics.hrl").

%% Updated utf-8: （・ω・）
-spec main(defined_type()) -> undefined_type().
main(X) -> X + 1.
