-module(els_indexer_SUITE).

%% CT Callbacks
-export([ all/0
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
        , do_not_skip_generated_file_by_tag_by_default/1
        , skip_generated_file_by_tag/1
        , skip_generated_file_by_custom_tag/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("els_core/include/els_core.hrl").

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
init_per_testcase(TestCase, Config) when
    TestCase =:= skip_generated_file_by_tag ->
  meck:new(els_config_indexing, [passthrough, no_link]),
  meck:expect(els_config_indexing, get_skip_generated_files, fun() -> true end),
  els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when
    TestCase =:= skip_generated_file_by_custom_tag ->
  meck:new(els_config_indexing, [passthrough, no_link]),
  meck:expect(els_config_indexing,
              get_skip_generated_files,
              fun() -> true end),
  meck:expect(els_config_indexing,
              get_generated_files_tag,
              fun() -> "@customgeneratedtag" end),
  els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) ->
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) when
    TestCase =:= skip_generated_file_by_tag ->
  meck:unload(els_config_indexing),
  els_test_utils:end_per_testcase(TestCase, Config);
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
  Path = filename:join(els_utils:to_binary(DataDir), "test.erl"),
  {ok, Uri} = els_indexing:shallow_index(Path, app),
  {ok, [#{id := test, kind := module}]} = els_dt_document:lookup(Uri),
  ok.

-spec index_hrl_file(config()) -> ok.
index_hrl_file(Config) ->
  DataDir = ?config(data_dir, Config),
  Path = filename:join(els_utils:to_binary(DataDir), "test.hrl"),
  {ok, Uri} = els_indexing:shallow_index(Path, app),
  {ok, [#{id := test, kind := header}]} = els_dt_document:lookup(Uri),
  ok.

-spec index_unkown_extension(config()) -> ok.
index_unkown_extension(Config) ->
  DataDir = ?config(data_dir, Config),
  Path = filename:join(els_utils:to_binary(DataDir), "test.foo"),
  {ok, Uri} = els_indexing:shallow_index(Path, app),
  {ok, [#{kind := other}]} = els_dt_document:lookup(Uri),
  ok.

-spec do_not_skip_generated_file_by_tag_by_default(config()) -> ok.
do_not_skip_generated_file_by_tag_by_default(Config) ->
  DataDir = data_dir(Config),
  GeneratedByTagUri = uri(DataDir, "generated_file_by_tag.erl"),
  GeneratedByCustomTagUri = uri(DataDir, "generated_file_by_custom_tag.erl"),
  ?assertEqual({4, 0, 0}, els_indexing:index_dir(DataDir, app)),
  {ok, [#{ id := generated_file_by_tag
         , kind := module
         }
       ]} = els_dt_document:lookup(GeneratedByTagUri),
  {ok, [#{ id := generated_file_by_custom_tag
         , kind := module
         }
       ]} = els_dt_document:lookup(GeneratedByCustomTagUri),
  ok.

-spec skip_generated_file_by_tag(config()) -> ok.
skip_generated_file_by_tag(Config) ->
  DataDir = data_dir(Config),
  GeneratedByTagUri = uri(DataDir, "generated_file_by_tag.erl"),
  GeneratedByCustomTagUri = uri(DataDir, "generated_file_by_custom_tag.erl"),
  ?assertEqual({3, 1, 0}, els_indexing:index_dir(DataDir, app)),
  {ok, []} = els_dt_document:lookup(GeneratedByTagUri),
  {ok, [#{ id := generated_file_by_custom_tag
         , kind := module
         }
       ]} = els_dt_document:lookup(GeneratedByCustomTagUri),
  ok.

-spec skip_generated_file_by_custom_tag(config()) -> ok.
skip_generated_file_by_custom_tag(Config) ->
  DataDir = data_dir(Config),
  GeneratedByTagUri = uri(DataDir, "generated_file_by_tag.erl"),
  GeneratedByCustomTagUri = uri(DataDir, "generated_file_by_custom_tag.erl"),
  ?assertEqual({3, 1, 0}, els_indexing:index_dir(DataDir, app)),
  {ok, [#{ id := generated_file_by_tag
         , kind := module
         }
       ]} = els_dt_document:lookup(GeneratedByTagUri),
  {ok, []} = els_dt_document:lookup(GeneratedByCustomTagUri),
  ok.

-spec data_dir(proplists:proplist()) -> binary().
data_dir(Config) ->
  ?config(data_dir, Config).

-spec uri(binary(), string()) -> uri().
uri(DataDir, FileName) ->
  Path = els_utils:to_binary(filename:join(DataDir, FileName)),
  els_uri:uri(Path).
