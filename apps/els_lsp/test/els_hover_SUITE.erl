-module(els_hover_SUITE).

-include("els_lsp.hrl").

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

%% Test cases
-export([ local_call_no_args/1
        , local_call_with_args/1
        , remote_call_multiple_clauses/1
        , no_poi/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
  [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
  [{group, tcp}, {group, stdio}].

-spec groups() -> [atom()].
groups() ->
  els_test_utils:groups(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
local_call_no_args(Config) ->
  Uri = ?config(hover_docs_caller_uri, Config),
  #{result := Result} = els_client:hover(Uri, 9, 7),
  ?assert(maps:is_key(contents, Result)),
  Contents = maps:get(contents, Result),
  Value = <<"## local_call/0">>,
  Expected = #{ kind  => <<"markdown">>
              , value => Value
              },
  ?assertEqual(Expected, Contents),
  ok.

local_call_with_args(Config) ->
  Uri = ?config(hover_docs_caller_uri, Config),
  #{result := Result} = els_client:hover(Uri, 12, 7),
  ?assert(maps:is_key(contents, Result)),
  Contents = maps:get(contents, Result),
  Value = <<"## local_call/2\n\n"
            "```erlang\n\n"
            "  local_call(Arg1, Arg2) \n\n"
            "```\n\n"
            "```erlang\n"
            "-spec local_call(integer(), any()) -> tuple();\n"
            "                (float(), any()) -> tuple().\n"
            "```">>,
  Expected = #{ kind  => <<"markdown">>
              , value => Value
              },
  ?assertEqual(Expected, Contents),
  ok.

remote_call_multiple_clauses(Config) ->
  Uri = ?config(hover_docs_caller_uri, Config),
  #{result := Result} = els_client:hover(Uri, 15, 15),
  ?assert(maps:is_key(contents, Result)),
  Contents = maps:get(contents, Result),
  Value = <<"## hover_docs:multiple_clauses/1\n\n"
            "```erlang\n\n"
            "  multiple_clauses(L) when is_list(L)\n\n"
            "  multiple_clauses(#{data := Data}) \n\n"
            "  multiple_clauses(X) \n\n```">>,
  Expected = #{ kind  => <<"markdown">>
              , value => Value
              },
  ?assertEqual(Expected, Contents),
  ok.

no_poi(Config) ->
  Uri = ?config(hover_docs_caller_uri, Config),
  #{result := Result} = els_client:hover(Uri, 10, 1),
  ?assertEqual(null, Result),
  ok.
