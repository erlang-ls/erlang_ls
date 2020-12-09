-module(rename_usage1).

-include("rename.hrl").

-behaviour(rename).

-export([rename_me/1]).

-spec rename_me(any()) -> any().
rename_me(x) ->
  ok;
rename_me(_) ->
  any.

rename_me_macro() ->
  {?RENAME_ME, ?RENAME_ME}.

rename_me_parametrized_macro() ->
  {?RENAME_ME_PARAMETRIZED(1), ?RENAME_ME_PARAMETRIZED(2)}.
