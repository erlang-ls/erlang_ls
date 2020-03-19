-module(els_initialization_SUITE).

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
-export([ initialize_default/1
        , initialize_custom_relative/1
        , initialize_custom_absolute/1
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

-spec all() -> [{group, stdio | tcp}].
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
init_per_testcase(_TestCase, Config) ->
  Transport = els_test_utils:get_group(Config),
  Started   = els_test_utils:start(Transport),
  [{started, Started} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec initialize_default(config()) -> ok.
initialize_default(Config) ->
  RootUri  = ?config(root_uri, Config),
  els_client:initialize(RootUri, []),
  Result = els_config:get(macros),
  Expected = [#{"name" => "DEFINED_WITHOUT_VALUE"},
              #{"name" => "DEFINED_WITH_VALUE", "value" => 1}],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_custom_relative(config()) -> ok.
initialize_custom_relative(Config) ->
  RootUri  = ?config(root_uri, Config),
  ConfigPath = <<"../rebar3_release/erlang_ls.config">>,
  InitOpts = #{ <<"erlang">>
              => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_config:get(macros),
  Expected = [#{"name" => "DEFINED_FOR_RELATIVE_TEST"}],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_custom_absolute(config()) -> ok.
initialize_custom_absolute(Config) ->
  RootUri  = ?config(root_uri, Config),
  ConfigPath = filename:join( els_uri:path(RootUri)
                            , "../rebar3_release/erlang_ls.config"),
  InitOpts = #{ <<"erlang">>
              => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_config:get(macros),
  Expected = [#{"name" => "DEFINED_FOR_RELATIVE_TEST"}],
  ?assertEqual(Expected, Result),
  ok.
