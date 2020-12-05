-module(rename_usage1).

-behaviour(rename).

-export([rename_me/1]).

rename_me(_) ->
  ok.
