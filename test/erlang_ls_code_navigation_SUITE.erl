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
-export([ application_local/1
        , application_remote/1
        , behaviour/1
        , include/1
        , include_lib/1
        , macro/1
        , macro_included/1
        , record/1
        , record_included/1
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
  [ application_local
  , application_remote
  , behaviour
  , include
  , include_lib
  , macro
  , macro_included
  , record
  , record_included
  ].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec application_local(config()) -> ok.
application_local(Config) ->
  Thing = #{info => {application, {function_b, 0}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {20, 0}, to => {20, 10}}, Range),
  ok.

-spec application_remote(config()) -> ok.
application_remote(Config) ->
  Thing = #{info => {application, {code_navigation_extra, do, 1}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>, Config), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
  ok.

-spec behaviour(config()) -> ok.
behaviour(Config) ->
  Thing = #{info => {behaviour, 'behaviour_a'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"behaviour_a.erl">>, Config), FullName),
  ?assertEqual(#{from => {0, 1}, to => {0, 1}}, Range),
  ok.

-spec include(config()) -> ok.
include(Config) ->
  Thing = #{info => {include, "code_navigation.hrl"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec include_lib(config()) -> ok.
include_lib(Config) ->
  Thing = #{info => {include_lib, "code_navigation/include/code_navigation.hrl"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  Thing = #{info => {macro, 'MACRO_A'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {14, 0}, to => {14, 0}}, Range),
  ok.

-spec macro_included(config()) -> ok.
macro_included(Config) ->
  Thing = #{info => {macro, 'INCLUDED_MACRO_A'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {2, 0}, to => {2, 0}}, Range),
  ok.

%% TODO: Additional constructors for POI
%% TODO: Navigation should return POI, not range
-spec record(config()) -> ok.
record(Config) ->
  Thing = #{info => {record_expr, "record_a"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {12, 0}, to => {12, 0}}, Range),
  ok.

-spec record_included(config()) -> ok.
record_included(Config) ->
  Thing = #{info => {record_expr, "included_record_a"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec full_path(binary(), binary(), config()) -> binary().
full_path(Dir, FileName, Config) ->
  filename:join([ ?config(app_dir, Config)
                , atom_to_binary(Dir, utf8)
                , FileName
                ]).

-spec goto_def(binary(), erlang_ls_poi:poi(), [string()]) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_def(FileName, Thing, Path) ->
  erlang_ls_code_navigation:goto_definition(FileName, Thing, Path).
