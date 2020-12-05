-module(rename_usage1).

-behaviour(rename).

-export([rename_me/1]).

-spec rename_me(any()) -> any().
rename_me(x) ->
  ok;
rename_me(_) ->
  any.
