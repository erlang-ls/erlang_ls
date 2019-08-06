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
        , macro/1
        , record/1
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  DataDir    = ?config(data_dir, Config),
  DataDirBin = list_to_binary(DataDir),
  [ {data_dir_bin, DataDirBin}
  , {include_path, [ filename:join([DataDirBin, "src"])
                   , filename:join([DataDirBin, "include"])
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
  [behaviour, macro, record].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec behaviour(config()) -> ok.
behaviour(Config) ->
  FileName = <<"behaviour_a.erl">>,
  Thing    = #{ info => {behaviour, 'behaviour_a'} },
  Path     = ?config(include_path, Config),
  {ok, FullName, Range} = goto_definition(FileName, Thing, Path),
  ?assertEqual(full_path(src, FileName, Config), FullName),
  ?assertEqual(#{from => {0, 1}, to => {0, 1}}, Range),
  ok.

-spec macro(config()) -> ok.
macro(Config) ->
  FileName = <<"code_navigation.erl">>,
  Thing    = #{ info => {macro, 'MACRO_A'} },
  Path     = ?config(include_path, Config),
  {ok, FullName, Range} = goto_definition(FileName, Thing, Path),
  ?assertEqual(full_path(src, FileName, Config), FullName),
  ?assertEqual(#{from => {11, 0}, to => {11, 0}}, Range),
  ok.

%% TODO: Additional constructors for POI
%% TODO: Navigation should return POI, not range
-spec record(config()) -> ok.
record(Config) ->
  FileName = <<"code_navigation.erl">>,
  Thing    = #{ info => {record_expr, "record_a"} },
  Path     = ?config(include_path, Config),
  {ok, FullName, Range} = goto_definition(FileName, Thing, Path),
  ?assertEqual(full_path(src, FileName, Config), FullName),
  ?assertEqual(#{from => {9, 0}, to => {9, 0}}, Range),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
full_path(Dir, FileName, Config) ->
  filename:join([ ?config(data_dir_bin, Config)
                , atom_to_binary(Dir, utf8)
                , FileName
                ]).

goto_definition(FileName, Thing, Path) ->
  erlang_ls_code_navigation:goto_definition(FileName, Thing, Path).

%% TODO: include
%% TODO: include_lib
%% TODO: recursive record
%% TODO: recursive macro
%% TODO: local function
%% TODO: remote function
%% TODO: bif
