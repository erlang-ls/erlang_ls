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
-spec hover_docs(config()) -> ok.
hover_docs(Config) ->
  Uri = ?config(code_navigation_extra_uri, Config),
  #{result := Result} = els_client:hover(Uri, 12, 26),
  ?assert(maps:is_key(contents, Result)),
  Contents = maps:get(contents, Result),
  ?assertEqual( #{ kind  => <<"markdown">>
                 , value => <<"-spec function_j() -> pos_integer()."
                              "\n\n"
                              "# code_navigation:function_j/0"
                              "\n\n"
                              "Such a wonderful function."
                              "\n\n">>
                 }
              , Contents),
  ok.

-spec hover_no_docs(config()) -> ok.
hover_no_docs(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = els_client:hover(Uri, 32, 18),
  ?assertEqual(null, Result),
  ok.
