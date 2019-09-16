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
        , export_entry/1
        , fun_local/1
        , fun_remote/1
        , import_entry/1
        , include/1
        , include_lib/1
        , macro/1
        , macro_lowercase/1
        , macro_included/1
        , macro_with_args/1
        , macro_with_args_included/1
        , record_access/1
        , record_access_included/1
        , record_expr/1
        , record_expr_included/1
        , type_application/1
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
  RootDir   = code:priv_dir(erlang_ls),
  AppDir    = filename:join([list_to_binary(RootDir), ?TEST_APP]),
  OtpPath   = erlang_ls_code_navigation:otp_path(),
  [ {app_dir, AppDir}
  , {include_path, [ filename:join([AppDir, "src"])
                   , filename:join([AppDir, "include"])
                   | OtpPath
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
  , export_entry
  , fun_local
  , fun_remote
  , import_entry
  , include
  , include_lib
  , macro
  , macro_lowercase
  , macro_included
  , macro_with_args
  , macro_with_args_included
  , record_access
  , record_access_included
  , record_expr
  , record_expr_included
  , type_application
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
  ?assertEqual(#{from => {24, 0}, to => {24, 10}}, Range),
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

-spec export_entry(config()) -> ok.
export_entry(Config) ->
  Thing = #{info => {exports_entry, {callback_a, 0}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {27, 0}, to => {27, 10}}, Range),
  ok.

-spec fun_local(config()) -> ok.
fun_local(Config) ->
  Thing = #{info => {implicit_fun, {function_b, 0}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {24, 0}, to => {24, 10}}, Range),
  ok.

-spec fun_remote(config()) -> ok.
fun_remote(Config) ->
  Thing = #{info => {implicit_fun, {code_navigation_extra, do, 1}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>, Config), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
  ok.

-spec import_entry(config()) -> ok.
import_entry(Config) ->
  Thing = #{info => {import_entry, {code_navigation_extra, do, 1}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>, Config), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
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
  ?assertEqual(#{from => {17, 0}, to => {17, 0}}, Range),
  ok.

-spec macro_lowercase(config()) -> ok.
macro_lowercase(Config) ->
  Thing = #{info => {macro, 'macro_A'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {44, 0}, to => {44, 0}}, Range),
  ok.

-spec macro_included(config()) -> ok.
macro_included(Config) ->
  Thing = #{info => {macro, 'INCLUDED_MACRO_A'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {2, 0}, to => {2, 0}}, Range),
  ok.

-spec macro_with_args(config()) -> ok.
macro_with_args(Config) ->
  Thing = #{info => {macro, 'MACRO_WITH_ARGS'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {18, 0}, to => {18, 0}}, Range),
  ok.

-spec macro_with_args_included(config()) -> ok.
macro_with_args_included(Config) ->
  Thing = #{info => {macro, 'assertEqual'}},
  Path  = ?config(include_path, Config),
  {ok, FullName, _Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(otp_app_path("stdlib", "include", "assert.hrl"), FullName),
  %% Do not assert on line number to avoid binding to a specific OTP version
  ok.

-spec record_access(config()) -> ok.
record_access(Config) ->
  Thing = #{info => {record_access, {"record_a", "_X"}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {15, 0}, to => {15, 0}}, Range),
  ok.

-spec record_access_included(config()) -> ok.
record_access_included(Config) ->
  Thing = #{info => {record_access, {"included_record_a", "_X"}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

%% TODO: Additional constructors for POI
%% TODO: Navigation should return POI, not range
-spec record_expr(config()) -> ok.
record_expr(Config) ->
  Thing = #{info => {record_expr, "record_a"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>, Config), FullName),
  ?assertEqual(#{from => {15, 0}, to => {15, 0}}, Range),
  ok.

-spec record_expr_included(config()) -> ok.
record_expr_included(Config) ->
  Thing = #{info => {record_expr, "included_record_a"}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>, Config), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec type_application(config()) -> ok.
type_application(Config) ->
  Thing = #{info => {type_application, {'type_a', undefined}}},
  Path  = ?config(include_path, Config),
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing, Path),
  ?assertEqual( full_path(src, <<"code_navigation.erl">>, Config)
              , FullName),
  ?assertEqual(#{from => {36, 1}, to => {36, 1}}, Range),
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

-spec otp_app_path(string(), string(), string()) -> binary().
otp_app_path(App, Dir, FileName) ->
  list_to_binary(filelib:wildcard(filename:join([ code:root_dir()
                                                , "lib"
                                                , App ++ "*"
                                                , Dir
                                                , FileName
                                                ]))).

-spec goto_def(binary(), erlang_ls_poi:poi(), [string()]) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_def(FileName, Thing, Path) ->
  erlang_ls_code_navigation:goto_definition(FileName, Thing, Path).
