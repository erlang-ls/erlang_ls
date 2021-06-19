-module(els_hover_SUITE).

-include("els_lsp.hrl").

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ local_call_no_args/1
        , local_call_with_args/1
        , remote_call_multiple_clauses/1
        , no_poi/1
        , included_macro/1
        , local_macro/1
        , weird_macro/1
        , macro_with_zero_args/1
        , macro_with_args/1
        , local_record/1
        , included_record/1
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
  els_test_utils:all(?MODULE).

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

local_macro(Config) ->
  Uri = ?config(hover_macro_uri, Config),
  #{result := Result} = els_client:hover(Uri, 6, 4),
  Value = <<"```erlang\n?LOCAL_MACRO = local_macro\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

included_macro(Config) ->
  Uri = ?config(hover_macro_uri, Config),
  #{result := Result} = els_client:hover(Uri, 7, 4),
  Value = <<"```erlang\n?INCLUDED_MACRO_A = included_macro_a\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

weird_macro(Config) ->
  Uri = ?config(hover_macro_uri, Config),
  #{result := Result} = els_client:hover(Uri, 12, 20),
  Value = <<"```erlang\n?WEIRD_MACRO = A when A > 1\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

macro_with_zero_args(Config) ->
  Uri = ?config(hover_macro_uri, Config),
  #{result := Result} = els_client:hover(Uri, 18, 10),
  Value = <<"```erlang\n?MACRO_WITH_ARGS() = {macro}\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

macro_with_args(Config) ->
  Uri = ?config(hover_macro_uri, Config),
  #{result := Result} = els_client:hover(Uri, 19, 10),
  Value = <<"```erlang\n?MACRO_WITH_ARGS(X, Y) = {macro, X, Y}\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

no_poi(Config) ->
  Uri = ?config(hover_docs_caller_uri, Config),
  #{result := Result} = els_client:hover(Uri, 10, 1),
  ?assertEqual(null, Result),
  ok.

local_record(Config) ->
  Uri = ?config(hover_record_expr_uri, Config),
  #{result := Result} = els_client:hover(Uri, 11, 4),
  Value = <<"```erlang\n-record(test_record, {\n"
            "        field1 = 123,\n"
            "        field2 = xyzzy,\n"
            "        field3\n"
            "}).\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.

included_record(Config) ->
  Uri = ?config(hover_record_expr_uri, Config),
  #{result := Result} = els_client:hover(Uri, 15, 4),
  Value = <<"```erlang\n"
            "-record(included_record_a, {included_field_a, included_field_b})."
            "\n```">>,
  Expected = #{contents => #{ kind  => <<"markdown">>
                            , value => Value
                            }},
  ?assertEqual(Expected, Result),
  ok.
