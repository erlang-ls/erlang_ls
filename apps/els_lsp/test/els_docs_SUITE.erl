-module(els_docs_SUITE).

-include("els_lsp.hrl").

%% CT Callbacks
-export([
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    memo_docs_true/1,
    memo_docs_false/1,
    invalidate/1
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
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec memo_docs_true(config()) -> ok.
memo_docs_true(Config) ->
    RootUri = els_test_utils:root_uri(),
    DataDir = ?config(data_dir, Config),
    ConfigPath = filename:join(DataDir, "docs_memo_true.config"),
    InitOpts = #{<<"erlang">> => #{<<"config_path">> => ConfigPath}},
    els_client:initialize(RootUri, InitOpts),

    %% Function
    MFACT1 = {M1 = docs_memo, F1 = function, A1 = 0, 'local', function},
    Expected1 = els_docs:function_docs('local', M1, F1, A1),
    {ok, [#{entries := Result1}]} = els_docs_memo:lookup(MFACT1),
    ?assertEqual(Expected1, Result1),

    %% Type
    MFACT2 = {M2 = docs_memo, F2 = type, A2 = 0, 'local', type},
    Expected2 = els_docs:type_docs('local', M2, F2, A2),
    {ok, [#{entries := Result2}]} = els_docs_memo:lookup(MFACT2),
    ?assertEqual(Expected2, Result2),

    ok.

-spec memo_docs_false(config()) -> ok.
memo_docs_false(Config) ->
    RootUri = els_test_utils:root_uri(),
    DataDir = ?config(data_dir, Config),
    ConfigPath = filename:join(DataDir, "docs_memo_false.config"),
    InitOpts = #{<<"erlang">> => #{<<"config_path">> => ConfigPath}},
    els_client:initialize(RootUri, InitOpts),

    %% Function
    MFACT1 = {M1 = docs_memo, F1 = function, A1 = 0, 'local', function},
    els_docs:function_docs('local', M1, F1, A1),
    {ok, []} = els_docs_memo:lookup(MFACT1),

    %% Type
    MFACT2 = {M2 = docs_memo, F2 = type, A2 = 0, 'local', type},
    els_docs:type_docs('local', M2, F2, A2),
    {ok, []} = els_docs_memo:lookup(MFACT2),

    ok.

-spec invalidate(config()) -> ok.
invalidate(Config) ->
    Uri = ?config(docs_memo_uri, Config),
    RootUri = els_test_utils:root_uri(),
    DataDir = ?config(data_dir, Config),
    ConfigPath = filename:join(DataDir, "docs_memo_true.config"),
    InitOpts = #{<<"erlang">> => #{<<"config_path">> => ConfigPath}},
    els_client:initialize(RootUri, InitOpts),
    meck:new(els_text_synchronization, [passthrough]),

    MFACT = {M = docs_memo, F = function, A = 0, 'local', function},

    %% Did save
    els_docs:function_docs('local', M, F, A),
    {ok, [_]} = els_docs_memo:lookup(MFACT),
    ok = els_client:did_save(Uri),
    els_test_utils:wait_until_mock_called(els_text_synchronization, did_save),
    {ok, []} = els_docs_memo:lookup(MFACT),

    %% Did change watched files
    els_docs:function_docs('local', M, F, A),
    {ok, [_]} = els_docs_memo:lookup(MFACT),
    ok = els_client:did_change_watched_files([{Uri, ?FILE_CHANGE_TYPE_CHANGED}]),
    els_test_utils:wait_until_mock_called(els_text_synchronization, did_change_watched_files),
    {ok, []} = els_docs_memo:lookup(MFACT),

    ok.
