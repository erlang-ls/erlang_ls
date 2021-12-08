-module(els_initialization_SUITE).

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
init_per_testcase(_TestCase, Config) ->
  meck:new(els_distribution_server, [no_link, passthrough]),
  meck:expect(els_distribution_server, connect, 0, ok),
  Started = els_test_utils:start(),
  [{started, Started} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec initialize_default(config()) -> ok.
initialize_default(_Config) ->
  RootUri  = els_test_utils:root_uri(),
  els_client:initialize(RootUri),
  Result = els_config:get(macros),
  Expected = [#{"name" => "DEFINED_WITHOUT_VALUE"},
              #{"name" => "DEFINED_WITH_VALUE", "value" => 1}],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_custom_relative(config()) -> ok.
initialize_custom_relative(_Config) ->
  RootUri  = els_test_utils:root_uri(),
  ConfigPath = <<"../rebar3_release/erlang_ls.config">>,
  InitOpts = #{ <<"erlang">>
              => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_config:get(macros),
  Expected = [#{"name" => "DEFINED_FOR_RELATIVE_TEST"}],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_custom_absolute(config()) -> ok.
initialize_custom_absolute(_Config) ->
  RootUri  = els_test_utils:root_uri(),
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
  RootUri = els_test_utils:root_uri(),
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
  RootUri = els_test_utils:root_uri(),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "diagnostics_custom.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = [ <<"bound_var_in_pattern">>
             , <<"compiler">>
             , <<"crossref">>
             , <<"dialyzer">>
             , <<"unused_includes">>
             , <<"unused_macros">>
             , <<"unused_record_fields">>
             ],
  Result = els_diagnostics:enabled_diagnostics(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_diagnostics_invalid(config()) -> ok.
initialize_diagnostics_invalid(Config) ->
  RootUri = els_test_utils:root_uri(),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "diagnostics_invalid.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_diagnostics:enabled_diagnostics(),
  Expected = [ <<"bound_var_in_pattern">>
             , <<"compiler">>
             , <<"crossref">>
             , <<"dialyzer">>
             , <<"elvis">>
             , <<"unused_includes">>
             , <<"unused_macros">>
             , <<"unused_record_fields">>
             ],
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_lenses_default(config()) -> ok.
initialize_lenses_default(Config) ->
  RootUri = els_test_utils:root_uri(),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_default.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = lists:sort(els_code_lens:default_lenses()),
  Result = els_code_lens:enabled_lenses(),
  ?assertEqual(Expected, lists:sort(Result)),
  ok.

-spec initialize_lenses_custom(config()) -> ok.
initialize_lenses_custom(Config) ->
  RootUri = els_test_utils:root_uri(),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_custom.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Expected = [ <<"function-references">>
             , <<"server-info">>
             , <<"suggest-spec">>
             ],
  Result = els_code_lens:enabled_lenses(),
  ?assertEqual(Expected, Result),
  ok.

-spec initialize_lenses_invalid(config()) -> ok.
initialize_lenses_invalid(Config) ->
  RootUri = els_test_utils:root_uri(),
  DataDir = ?config(data_dir, Config),
  ConfigPath = filename:join(DataDir, "lenses_invalid.config"),
  InitOpts = #{ <<"erlang">> => #{ <<"config_path">> => ConfigPath }},
  els_client:initialize(RootUri, InitOpts),
  Result = els_code_lens:enabled_lenses(),
  Expected = [ <<"ct-run-test">>
             , <<"function-references">>
             , <<"show-behaviour-usages">>
             , <<"suggest-spec">>
             ],
  ?assertEqual(Expected, Result),
  ok.
