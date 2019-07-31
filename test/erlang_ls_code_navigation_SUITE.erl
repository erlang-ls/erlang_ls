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
-export([ macro/1
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
  [macro, record].

%%==============================================================================
%% Testcases
%%==============================================================================
-spec macro(config()) -> ok.
macro(Config) ->
  FileName    = <<"code_navigation.erl">>,
  Thing       = {macro, 'MACRO_A'},
  Definition  = definition(?config(data_dir_bin, Config), Thing),
  ?assertEqual( Definition
              , erlang_ls_server:search( FileName
                                       , ?config(include_path, Config)
                                       , erlang_ls_server:definition(Thing))),
  ok.

-spec record(config()) -> ok.
record(Config) ->
  FileName    = <<"code_navigation.erl">>,
  Thing       = {record_expr, "record_a"},
  Definition  = definition(?config(data_dir_bin, Config), Thing),
  ?assertEqual( Definition
              , erlang_ls_server:search( FileName
                                       , ?config(include_path, Config)
                                       , erlang_ls_server:definition(Thing))),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
definition(DataDir, {record_expr, "record_a"}) ->
  FilePath = filename:join([DataDir, <<"src">>, <<"code_navigation.erl">>]),
  #{ range => erlang_ls_protocol:range(#{from => {4, 0}, to => {4, 0}})
   , uri   => erlang_ls_uri:uri(FilePath)
   };
definition(DataDir, {macro, 'MACRO_A'}) ->
  FilePath = filename:join([DataDir, <<"src">>, <<"code_navigation.erl">>]),
  #{ range => erlang_ls_protocol:range(#{from => {6, 0}, to => {6, 0}})
   , uri   => erlang_ls_uri:uri(FilePath)
   }.

%% TODO: Armonize strings vs atoms in definitions
