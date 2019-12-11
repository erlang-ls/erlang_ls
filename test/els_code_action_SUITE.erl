-module(els_code_action_SUITE).

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
-export([ add_todo_comment/1
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
-spec add_todo_comment(config()) -> ok.
add_todo_comment(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  application:set_env(erlang_ls, test_code_action, true),
  Res = els_client:document_codeaction
          (Uri, els_protocol:range(#{from => {22, 3}, to => {22, 13}}), []),
  #{ result := Result } = Res,
  Expected = [#{command =>
                         #{arguments =>
                               [#{before => 21,
                                  lines => <<"%% TODO: something\n">>,
                                  uri => Uri}],
                           command => <<"add-lines">>,
                           title => <<"Add TODO comment">>},
                     title => <<"Add TODO comment">>}],
  ?assertEqual(Result,Expected),
  ok.


%%==============================================================================
%% Internal functions
%%==============================================================================

