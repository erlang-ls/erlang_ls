%%==============================================================================
%% Tests for the 'progress' functionality
%%==============================================================================
-module(els_progress_SUITE).

%%==============================================================================
%% Common Test Callbacks
%%==============================================================================
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

%%==============================================================================
%% Testcases
%%==============================================================================
-export([ sample_job/1
        , failing_job/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("erlang_ls.hrl").

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

-spec sample_job(config()) -> ok.
sample_job(_Config) ->
  Entries = lists:seq(1, 27),
  %% TODO: Move to init/end
  meck:new(sample_job, [non_strict]),
  meck:expect(sample_job, task, fun(_) -> ok end),
  meck:expect(sample_job, on_complete, fun() -> ok end),
  %% TODO: Proper API to start a job
  Config = #{ task => fun sample_job:task/1
            , entries => Entries
            , on_complete => fun sample_job:on_complete/0
            , title => <<"Sample job">>
            },
  %% TODO: Accept parameter with progress type ($/progress or showMessage)
  {ok, Pid} = supervisor:start_child(els_background_job_sup, [Config]),
  wait_for_completion(Pid),
  ?assertEqual(length(Entries), meck:num_calls(sample_job, task, '_')),
  ?assertEqual(1, meck:num_calls(sample_job, on_complete, '_')),
  ok.

-spec failing_job(config()) -> ok.
failing_job(_Config) ->
  Entries = lists:seq(1, 27),
  %% TODO: Move to init/end
  meck:new(sample_job, [non_strict]),
  meck:expect(sample_job, task, fun(_) -> throw(fail) end),
  meck:expect(sample_job, on_complete, fun() -> ok end),
  %% TODO: Proper API to start a job
  Config = #{ task => fun sample_job:task/1
            , entries => Entries
            , on_complete => fun sample_job:on_complete/0
            , title => <<"Sample job">>
            },
  %% TODO: Accept parameter with progress type ($/progress or showMessage)
  {ok, Pid} = supervisor:start_child(els_background_job_sup, [Config]),
  wait_for_completion(Pid),
  ?assertEqual(1, meck:num_calls(sample_job, task, '_')),
  ?assertEqual(0, meck:num_calls(sample_job, on_complete, '_')),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec wait_for_completion(pid()) -> ok.
wait_for_completion(Pid) ->
  case is_process_alive(Pid) of
    false ->
      ok;
    true ->
      timer:sleep(10)
  end.
