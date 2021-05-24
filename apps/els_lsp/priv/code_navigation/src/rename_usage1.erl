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

rename_record(R = #rename_rec{rename_field = F}) ->
  %% field access
  F = R#rename_rec.rename_field,
  %% record create
  R2 = #rename_rec{rename_field = 12},
  %% record update
  R2#rename_rec{rename_field = F}.
