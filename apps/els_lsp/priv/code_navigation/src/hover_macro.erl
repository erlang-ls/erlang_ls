-module(hover_macro).
-include("code_navigation.hrl").
-define(LOCAL_MACRO, local_macro).

f() ->
  ?LOCAL_MACRO,
  ?INCLUDED_MACRO_A.
