-module(completion_attributes).
-behaviour(behaviour_x).
-include("definition.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([exported_function/0]).
-export_type([exported_type/0]).
-export_type([exported_opaque/0]).

-type exported_type() :: any().
-type unexported_type() :: any().
-opaque exported_opaque() :: any().
-opaque unexported_opaque() :: any().

exported_function() ->
  ok.

unexported_function() ->
  ok.

-ifdef(MACRO).
-endif.

-ifndef(MACRO).
-endif.
