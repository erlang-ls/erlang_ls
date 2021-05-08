-module(code_navigation_undefined).

-export([f0/0, f1/1]).

f0() ->
  #undef_rec{undef_field = ?UNDEF_MACRO}.

f1(#undef_rec{undef_field = ?UNDEF_MACRO}) ->
  ok.
