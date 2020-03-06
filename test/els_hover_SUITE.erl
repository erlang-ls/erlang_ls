-module(els_hover_SUITE).

-include("erlang_ls.hrl").

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
-export([ hover_docs/1
        , hover_docs_local/1
        , hover_no_docs/1
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

-define( FUNCTION_J_DOC
       , <<"```erlang\n"
           "-spec function_j() -> pos_integer().\n"
           "```\n\n"
           "### code_navigation:function_j/0"
           "\n\n"
           "Such a wonderful function."
           "\n\n">>
       ).

-spec hover_docs(config()) -> ok.
hover_docs(Config) ->
  Uri = ?config(code_navigation_extra_uri, Config),
  #{result := Result} = els_client:hover(Uri, 13, 26),
  ?assert(maps:is_key(contents, Result)),
  Contents = maps:get(contents, Result),
  Expected = #{ kind  => <<"markdown">>
              , value => ?FUNCTION_J_DOC
              },

  ?assertEqual(Expected, Contents),
  ok.

hover_docs_local(Config) ->
  ct:comment("Hover the local function call"),
  ExtraUri = ?config(code_navigation_extra_uri, Config),
  Response1 = els_client:hover(ExtraUri, 6, 5),
  ?assertMatch(#{result := #{contents := _}}, Response1),
  #{result := #{contents := Contents1}} = Response1,
  Expected1 = #{ kind  => <<"markdown">>
               , value => <<"```erlang\n"
                            "-spec do_4(nat(), opaque_local()) -> {atom(),"
                            "\n\t\t\t\t"
                            "      code_navigation_types:opaque_type_a()}.\n"
                            "```\n\n"
                            "### code_navigation_extra:do_4/2"
                            "\n\ndo_4 is a local-only function"
                            "\n\n">>
               },
  ?assertEqual(Expected1, Contents1),

  ct:comment("Hover the export entry for function_j/0"),
  Uri = ?config(code_navigation_uri, Config),
  Response2 = els_client:hover(Uri, 5, 55),
  ?assertMatch(#{result := #{contents := _}}, Response2),
  #{result := #{contents := Contents2}} = Response2,
  Expected2 =#{kind  => <<"markdown">>, value => ?FUNCTION_J_DOC},
  ?assertEqual(Expected2, Contents2),
  ok.

-spec hover_no_docs(config()) -> ok.
hover_no_docs(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = els_client:hover(Uri, 32, 18),
  ?assertEqual(null, Result),
  ok.
