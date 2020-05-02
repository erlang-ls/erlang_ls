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
        , initialize_diagnostics_default/1
        , initialize_diagnostics_custom/1
        , initialize_diagnostics_invalid/1
        , initialize_lenses_default/1
        , initialize_lenses_custom/1
        , initialize_lenses_invalid/1
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
  meck:new(els_build_server, [no_link, passthrough]),
  meck:expect(els_build_server, connect, 0, ok),
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
  els_client:initialize(RootUri),
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

-spec initialize_diagnostics_default(config()) -> ok.
initialize_diagnostics_default(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "diagnostics_default.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = els_diagnostics:default_diagnostics(),
  Result = els_diagnostics:enabled_diagnostics(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_diagnostics_custom(config()) -> ok.
initialize_diagnostics_custom(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "diagnostics_custom.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = [<<"compiler">>, <<"dialyzer">>, <<"xref">>],
  Result = els_diagnostics:enabled_diagnostics(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_diagnostics_invalid(config()) -> ok.
initialize_diagnostics_invalid(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "diagnostics_invalid.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_diagnostics:enabled_diagnostics(),
  Expected = [<<"compiler">>, <<"dialyzer">>, <<"elvis">>, <<"xref">>],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_lenses_default(config()) -> ok.
initialize_lenses_default(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_default.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = els_code_lens:default_lenses(),
  Result = els_code_lens:enabled_lenses(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_lenses_custom(config()) -> ok.
initialize_lenses_custom(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_custom.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = [<<"server-info">>],
  Result = els_code_lens:enabled_lenses(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_lenses_invalid(config()) -> ok.
initialize_lenses_invalid(Config) ->
  RootUri = ?config(root_uri, Config),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_invalid.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_code_lens:enabled_lenses(),
  Expected = [<<"ct-run-test">>, <<"show-behaviour-usages">>],
  ?assertEqual(Expected, Result),
  ok.
