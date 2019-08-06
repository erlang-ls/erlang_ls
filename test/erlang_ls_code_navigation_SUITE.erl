%%==============================================================================
%% Unit Tests for Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ behaviour/1
        , include/1
        , macro/1
        , record/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).

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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  RootDir = code:priv_dir(erlang_ls),
  AppDir  = filename:join([list_to_binary(RootDir), ?TEST_APP]),
  [ {app_dir, AppDir}
  , {include_path, [ filename:join([AppDir, "src"])
                   , filename:join([AppDir, "include"])
                   ]}
    |Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok.

-spec all() -> [atom()].
all() ->
  [behaviour, include, macro, record].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec behaviour(config()) -> ok.
behaviour(Config) ->
  Thing = #{ info => {behaviour, 'behaviour_a'} },
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"behaviour_a.erl">>, Config), FullName),
  ?assertEqual(#{from => {0, 1}, to => {0, 1}}, Range),
  ok.

-spec include(config()) -> ok.
include(Config) ->
  Thing = #{ info => {include, "code_navigation.hrl"} },
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  Thing = #{ info => {macro, 'MACRO_A'} },
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {13, 0}, to => {13, 0}}, Range),
  ok.

%% TODO: Additional constructors for POI
%% TODO: Navigation should return POI, not range
-spec record(config()) -> ok.
record(Config) ->
  Thing = #{ info => {record_expr, "record_a"} },
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {11, 0}, to => {11, 0}}, Range),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
full_path(Dir, FileName, Config) ->
  filename:join([ ?config(app_dir, Config)
                , atom_to_binary(Dir, utf8)
                , FileName
                ]).

goto_def(FileName, Thing, Path) ->
  erlang_ls_code_navigation:goto_definition(FileName, Thing, Path).

%% TODO: include
%% TODO: include_lib
%% TODO: recursive record
%% TODO: recursive macro
%% TODO: local function
%% TODO: remote function
%% TODO: bif
