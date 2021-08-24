-module(diagnostics_unused_macros).

-export([main/0]).

-define(USED_MACRO, used_macro).
-define(UNUSED_MACRO, unused_macro).
-define(UNUSED_MACRO_WITH_ARG(C), C).
-define(MOD, module). %% MOD was incorrectly reported as unused (#1021)

main() ->
  ?MOD:foo(),
  ?USED_MACRO.
