-module(els_indexing_SUITE).

%% CT Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ index_otp/1
        , reindex_otp/1
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
  {ok, Started} = application:ensure_all_started(erlang_ls),
  RootDir = code:root_dir(),
  RootUri = els_uri:uri(list_to_binary(RootDir)),
  %% Do not index the entire list of OTP apps in the pipelines.
  %% This portion of code is stolen from els_config, which needs some
  %% serious refactoring.
  %% ExcludePathsSpecs = [[RootDir, "lib", P ++ "*"] || P <- otp_apps_exclude()]
  %% RootPath = binary_to_list(els_uri:path(RootUri)),
  %% ExcludePaths = els_utils:resolve_paths(ExcludePathsSpecs, RootPath, true),
  %% erlang:display(ExcludePaths),
  Cfg = #{"otp_apps_exclude" => otp_apps_exclude()},
  els_config:do_initialize(RootUri, [], Cfg),
  [{started, Started}|Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec index_otp(config()) -> ok.
index_otp(Config) ->
  index_otp(db, ?config(priv_dir, Config)).

-spec reindex_otp(config()) -> ok.
reindex_otp(Config) ->
  index_otp(db, ?config(priv_dir, Config)),
  ok.

-spec index_otp(atom(), string()) -> ok.
index_otp(DBName, DBDir) ->
  ok = els_db:install(DBName, DBDir),
  els_indexer:index_dirs(els_config:get(otp_paths), 'shallow'),
  ok = els_db:stop().

-spec otp_apps_exclude() -> [string()].
otp_apps_exclude() ->
  [ "asn1"
  , "common_test"
  , "compiler"
  , "crypto"
  , "debugger"
  , "dialyzer"
  , "diameter"
  , "edoc"
  , "eldap"
  , "erl_docgen"
  , "erl_interface"
  , "et"
  , "eunit"
  , "ftp"
  , "inets"
  , "jinterface"
  , "megaco"
  , "mnesia"
  , "observer"
  , "os_mon"
  , "otp_mibs"
  , "parsetools"
  , "reltool"
  , "sasl"
  , "snmp"
  , "ssh"
  , "ssl"
  , "syntax_tools"
  , "tftp"
  , "xmerl"
  , "wx"
  ].
