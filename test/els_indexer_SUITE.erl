-module(els_indexer_SUITE).

%% CT Callbacks
-export([ all/0
        , groups/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ index_dir_not_dir/1
        , index_erl_file/1
        , index_hrl_file/1
        , index_unkown_extension/1
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
-spec all() -> [{group, atom()}].
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
-spec index_dir_not_dir(config()) -> ok.
index_dir_not_dir(Config) ->
  DataDir    = ?config(data_dir, Config),
  NotDirPath = filename:join(DataDir, "not_a_dir"),
  file:write_file(NotDirPath, <<>>),
  ok = els_utils:fold_files( fun(_, _) -> ok end
                           , fun(_) -> true end
                           , NotDirPath
                           , ok
                           ),
  file:delete(NotDirPath),
  ok.

-spec index_erl_file(config()) -> ok.
index_erl_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Path = filename:join(unicode:characters_to_binary(DataDir), "test.erl"),
  {ok, Uri} = els_indexing:index_file(Path),
  {ok, [#{id := test, kind := module}]} = els_dt_document:lookup(Uri),
  ok.

-spec index_hrl_file(config()) -> ok.
index_hrl_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Path = filename:join(unicode:characters_to_binary(DataDir), "test.hrl"),
  {ok, Uri} = els_indexing:index_file(Path),
  {ok, [#{id := test, kind := header}]} = els_dt_document:lookup(Uri),
  ok.

-spec index_unkown_extension(config()) -> ok.
index_unkown_extension(Config) ->
  DataDir = ?config(data_dir, Config),
  Path = filename:join(unicode:characters_to_binary(DataDir), "test.foo"),
  {ok, Uri} = els_indexing:index_file(Path),
  {ok, [#{kind := other}]} = els_dt_document:lookup(Uri),
  ok.
