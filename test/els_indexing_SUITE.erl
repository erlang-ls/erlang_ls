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
  application:ensure_all_started(erlang_ls),
  application:set_env(erlang_ls, index_otp, true),
  RootUri = els_uri:uri(list_to_binary(code:root_dir())),
  els_config:initialize(RootUri, [], []),
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  %% TODO: The transport should be included in the OTP supervision
  %% tree, so it can be restarted.
  ok = ranch:stop_listener(erlang_ls),
  application:stop(erlang_ls),
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
  ok = els_indexer:index_otp(),
  Entries = mnesia:table_info(els_dt_references, size),
  Size = mnesia:table_info(els_dt_references, memory) * 8 / 1024 / 1024,
  ct:comment("Entries: ~p. Size (MB): ~p", [Entries, Size]),
  %% mnesia_controller:snapshot_dcd(tables()), %% Seems to save 3-4 seconds
  ok = els_db:stop().

%% %% TODO: Move to els_db
%% tables() ->
%%   [ els_dt_document
%%   , els_dt_document_index
%%   , els_dt_references
%%   , els_dt_signatures
%%   ].
