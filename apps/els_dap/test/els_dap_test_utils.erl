-module(els_dap_test_utils).

-export([ all/1
        , all/2
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        , wait_for/2
        , wait_for_fun/3
        ]).

-include_lib("common_test/include/ct.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% API
%%==============================================================================

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  PrivDir = code:priv_dir(els_dap),
  RootPath = filename:join([ els_utils:to_binary(PrivDir)
                           , ?TEST_APP]),
  RootUri = els_uri:uri(RootPath),
  application:load(els_core),
  [ {root_uri, RootUri}
  , {root_path, RootPath}
  | Config ].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  meck:new(els_distribution_server, [no_link, passthrough]),
  meck:expect(els_distribution_server, connect, 0, ok),
  Started = els_test_utils:start(),
  RootUri = ?config(root_uri, Config),
  els_client:initialize(RootUri, #{indexingEnabled => false}),
  els_client:initialized(),
[ {started, Started}
                  | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  meck:unload(els_distribution_server),
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec wait_for(any(), non_neg_integer()) -> ok.
wait_for(_Message, Timeout) when Timeout =< 0 ->
  timeout;
wait_for(Message, Timeout) ->
  receive Message -> ok
  after 10 -> wait_for(Message, Timeout - 10)
  end.

-spec wait_for_fun(term(), non_neg_integer(), non_neg_integer()) ->
        {ok, any()} | ok | timeout.
wait_for_fun(_CheckFun, _WaitTime, 0) ->
  timeout;
wait_for_fun(CheckFun, WaitTime, Retries) ->
  case CheckFun() of
    true ->
      ok;
    {true, Value} ->
      {ok, Value};
    false ->
      timer:sleep(WaitTime),
      wait_for_fun(CheckFun, WaitTime, Retries - 1)
  end.
