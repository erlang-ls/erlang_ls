-module(els_code_lens_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

%% Test cases
-export([ get_erlang_ls_info/1
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
-spec get_erlang_ls_info(config()) -> ok.
get_erlang_ls_info(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Result} = els_client:document_codelens(Uri),
  PrefixedCommand
    = els_execute_command_provider:add_server_prefix(<<"info">>),
  Title = <<"Erlang LS (in code_navigation) info">>,
  Expected =
    [ #{ command => #{ arguments => [ #{ uri => Uri } ]
                     , command   => PrefixedCommand
                     , title     => Title
                     }
       , range =>
           #{'end' => #{character => 0, line => 1},
             start => #{character => 0, line => 0}}
       }
    ],
  ?assertEqual(Expected, Result),
  ok.
