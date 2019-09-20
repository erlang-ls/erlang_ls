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
  {ok, Started} = application:ensure_all_started(erlang_ls),
  erlang_ls_buffer_server:set_otp_path(code:root_dir()),
  erlang_ls_buffer_server:set_root_uri(erlang_ls_uri:uri(root_path())),
  [{started, Started}|Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
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
application_local(_Config) ->
  Thing = #{info => {application, {function_b, 0}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {24, 0}, to => {24, 10}}, Range),
  ok.

-spec application_remote(config()) -> ok.
application_remote(_Config) ->
  Thing = #{info => {application, {code_navigation_extra, do, 1}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
  ok.

-spec behaviour(config()) -> ok.
behaviour(_Config) ->
  Thing = #{info => {behaviour, 'behaviour_a'}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"behaviour_a.erl">>), FullName),
  ?assertEqual(#{from => {0, 1}, to => {0, 1}}, Range),
  ok.

-spec export_entry(config()) -> ok.
export_entry(_Config) ->
  Thing = #{info => {exports_entry, {callback_a, 0}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {27, 0}, to => {27, 10}}, Range),
  ok.

-spec fun_local(config()) -> ok.
fun_local(_Config) ->
  Thing = #{info => {implicit_fun, {function_b, 0}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {24, 0}, to => {24, 10}}, Range),
  ok.

-spec fun_remote(config()) -> ok.
fun_remote(_Config) ->
  Thing = #{info => {implicit_fun, {code_navigation_extra, do, 1}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
  ok.

-spec import_entry(config()) -> ok.
import_entry(_Config) ->
  Thing = #{info => {import_entry, {code_navigation_extra, do, 1}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation_extra.erl">>), FullName),
  ?assertEqual(#{from => {4, 0}, to => {4, 2}}, Range),
  ok.

-spec include(config()) -> ok.
include(_Config) ->
  Thing = #{info => {include, "code_navigation.hrl"}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec include_lib(config()) -> ok.
include_lib(_Config) ->
  Thing = #{info => {include_lib, "code_navigation/include/code_navigation.hrl"}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec macro(config()) -> ok.
macro(_Config) ->
  Thing = #{info => {macro, 'MACRO_A'}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {17, 0}, to => {17, 0}}, Range),
  ok.

-spec macro_lowercase(config()) -> ok.
macro_lowercase(_Config) ->
  Thing = #{info => {macro, 'macro_A'}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {44, 0}, to => {44, 0}}, Range),
  ok.

-spec macro_included(config()) -> ok.
macro_included(_Config) ->
  Thing = #{info => {macro, 'INCLUDED_MACRO_A'}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>), FullName),
  ?assertEqual(#{from => {2, 0}, to => {2, 0}}, Range),
  ok.

-spec macro_with_args(config()) -> ok.
macro_with_args(_Config) ->
  Thing = #{info => {macro, 'MACRO_WITH_ARGS'}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {18, 0}, to => {18, 0}}, Range),
  ok.

-spec macro_with_args_included(config()) -> ok.
macro_with_args_included(_Config) ->
  Thing = #{info => {macro, 'assertEqual'}},
  {ok, FullName, _Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(otp_app_path("stdlib", "include", "assert.hrl"), FullName),
  %% Do not assert on line number to avoid binding to a specific OTP version
  ok.

-spec record_access(config()) -> ok.
record_access(_Config) ->
  Thing = #{info => {record_access, {"record_a", "_X"}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {15, 0}, to => {15, 0}}, Range),
  ok.

-spec record_access_included(config()) -> ok.
record_access_included(_Config) ->
  Thing = #{info => {record_access, {"included_record_a", "_X"}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

%% TODO: Additional constructors for POI
%% TODO: Navigation should return POI, not range
-spec record_expr(config()) -> ok.
record_expr(_Config) ->
  Thing = #{info => {record_expr, "record_a"}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {15, 0}, to => {15, 0}}, Range),
  ok.

-spec record_expr_included(config()) -> ok.
record_expr_included(_Config) ->
  Thing = #{info => {record_expr, "included_record_a"}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual(full_path(include, <<"code_navigation.hrl">>), FullName),
  ?assertEqual(#{from => {0, 0}, to => {0, 0}}, Range),
  ok.

-spec type_application(config()) -> ok.
type_application(_Config) ->
  Thing = #{info => {type_application, {'type_a', undefined}}},
  {ok, FullName, Range} = goto_def(<<"code_navigation.erl">>, Thing),
  ?assertEqual( full_path(src, <<"code_navigation.erl">>), FullName),
  ?assertEqual(#{from => {36, 1}, to => {36, 1}}, Range),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec full_path(binary(), binary()) -> binary().
full_path(Dir, FileName) ->
  filename:join([ root_path()
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

-spec goto_def(binary(), erlang_ls_poi:poi()) ->
   {ok, binary(), erlang_ls_poi:range()} | {error, any()}.
goto_def(FileName, Thing) ->
  erlang_ls_code_navigation:goto_definition(FileName, Thing).

-spec root_path() -> file:filename().
root_path() ->
  filename:join([list_to_binary(code:priv_dir(erlang_ls)), ?TEST_APP]).
