-module(els_execute_command_SUITE).

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
-export([ erlang_ls_info/1
        , strip_server_prefix/1
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

-spec erlang_ls_info(config()) -> ok.
erlang_ls_info(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  PrefixedCommand = els_command:with_prefix(<<"server-info">>),
  #{result := Result}
    = els_client:workspace_executecommand(PrefixedCommand, [#{uri => Uri}]),
  Expected = [],
  ?assertEqual(Expected, Result),
  CheckFun = fun() -> case els_client:get_notifications() of
                        [] -> false;
                        Notifications -> {true, Notifications}
                      end
             end,
  {ok, [Notification]} = els_test_utils:wait_for_fun(CheckFun, 10, 3),
  ?assertEqual(maps:get(method, Notification), <<"window/showMessage">>),
  Params = maps:get(params, Notification),
  ?assertEqual(<<"Erlang LS (in code_navigation), ">>
              , binary:part(maps:get(message, Params), 0, 32)),
  ok.

-spec strip_server_prefix(config()) -> ok.
strip_server_prefix(_Config) ->
  PrefixedCommand = els_command:with_prefix(<<"server-info">>),
  ?assertEqual( <<"server-info">>
              , els_command:without_prefix(PrefixedCommand)),

  ?assertEqual( <<"server-info">>
              , els_command:without_prefix(<<"123:server-info">>)),

  ?assertEqual( <<"server-info">>
              , els_command:without_prefix(<<"server-info">>)),

  ?assertEqual( <<"server-info:f">>
              , els_command:without_prefix(<<"13:server-info:f">>)),
  ok.
