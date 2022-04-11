-module(els_server_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ cancel_request/1
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
init_per_testcase(cancel_request = TestCase, Config0) ->
  Config = els_test_utils:init_per_testcase(TestCase, Config0),
  %% Ensure the background job triggered by the code lens will never return
  meck:new(els_background_job, [no_link, passthrough]),
  meck:expect(els_background_job, new,
              fun(C0) ->
                  C = C0#{ task => fun(_, _) ->
                                       timer:sleep(infinity)
                                   end},
                  meck:passthrough([C])
              end),
  [ {mocks, [els_background_job]} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  Mocks = ?config(mocks, Config),
  [meck:unload(Mock) || Mock <- Mocks],
  els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec cancel_request(config()) -> ok.
cancel_request(Config) ->
  %% Trigger a document/CodeLens request in the background
  Uri = ?config(code_navigation_uri, Config),
  spawn(fun() -> els_client:document_codelens(Uri) end),
  %% Extract the current background job (from the lens provider)
  Job = wait_until_one_lens_job(),
  %% Verify the background job is triggered
  ?assert(lists:member(Job, els_background_job:list())),
  %% Cancel the original request
  Result = els_client:'$_cancelrequest'(),
  %% Ensure the previous request is canceled
  wait_until_no_lens_jobs(),
  ?assertEqual(ok, Result).

wait_until_one_lens_job() ->
  Jobs = get_current_lens_jobs(),
  case Jobs of
    [Job] ->
      Job;
    [] ->
      timer:sleep(100),
      wait_until_one_lens_job()
  end.

-spec wait_until_no_lens_jobs() -> ok.
wait_until_no_lens_jobs() ->
  case get_current_lens_jobs() of
    [] ->
      ok;
    _ ->
      timer:sleep(100),
      wait_until_no_lens_jobs()
  end.

-spec get_current_lens_jobs() -> [pid()].
get_current_lens_jobs() ->
  #{internal_state := InternalState} =
    sys:get_state(els_provider, 30 * 1000),
  #{in_progress := InProgress} = InternalState,
  [Job || {_Uri, Job} <- InProgress].
