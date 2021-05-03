-module(hover_macro).
-include("code_navigation.hrl").
-define(LOCAL_MACRO, local_macro).

f() ->
  ?LOCAL_MACRO,
  ?INCLUDED_MACRO_A.

-define(WEIRD_MACRO, A when A > 1).

g() ->
  case foo of ?WEIRD_MACRO -> ok end.
