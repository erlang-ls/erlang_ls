-module(signature_help).

-export([min/2,maps_get/0,record_max/0,multiline/0]).

min(A, B) ->
    erlang:min(A, B).

maps_get() ->
    maps:get(key, #{}, false).

record_max() ->
    erlang:max({a, b, c}, {d, e, f}).

multiline() ->
    erlang:min(
      1,
      2
    ).
