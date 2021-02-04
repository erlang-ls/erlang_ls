-module(rename_usage2).

-include("rename.hrl").

-behaviour(rename).

-export([rename_me/1]).

rename_me(_) ->
  ok.

rename_me_macro(?RENAME_ME) ->
  ok.

rename_me_parametrized_macro(?RENAME_ME_PARAMETRIZED(42)) ->
  ok.
