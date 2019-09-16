-module(code_navigation).

-behaviour(behaviour_a).

-export([ function_a/0 ]).

%% behaviour_a callbacks
-export([ callback_a/0 ]).

-import(code_navigation_extra, [ do/1 ]).

-include("code_navigation.hrl").
-include_lib("code_navigation/include/code_navigation.hrl").
-include_lib("stdlib/include/assert.hrl").

-record(record_a, {field_a, field_b}).

-define(MACRO_A, macro_a).
-define(MACRO_WITH_ARGS(X), erlang:display(X)).

function_a() ->
  function_b(),
  #record_a{}.

function_b() ->
  ?MACRO_A.

callback_a() ->
  ok.

function_c() ->
  code_navigation_extra:do(test),
  length([1, 2, 3]).

-type type_a() :: any().

function_d() ->
  ?MACRO_WITH_ARGS(d).

function_e() ->
  ?assertEqual(2.0, 4/2).
