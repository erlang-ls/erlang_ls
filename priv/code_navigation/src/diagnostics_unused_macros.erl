-module(diagnostics_unused_macros).

-export([main/0]).

-define(USED_MACRO, used_macro).
-define(UNUSED_MACRO, unused_macro).

main() ->
  ?USED_MACRO.
