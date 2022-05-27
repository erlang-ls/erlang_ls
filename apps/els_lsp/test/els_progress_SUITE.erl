%%==============================================================================
%% Tests for the 'progress' functionality
%%==============================================================================
-module(els_progress_SUITE).

%%==============================================================================
%% Common Test Callbacks
%%==============================================================================
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%%==============================================================================
%% Testcases
%%==============================================================================
-export([
    sample_job/1,
    failing_job/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-include("els_lsp.hrl").

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
init_per_testcase(sample_job = TestCase, Config) ->
    Task = fun(_, _) -> ok end,
    setup_mocks(Task),
    [{task, Task} | els_test_utils:init_per_testcase(TestCase, Config)];
init_per_testcase(failing_job = TestCase, Config) ->
    Task = fun(_, _) -> exit(fail) end,
    setup_mocks(Task),
    [{task, Task} | els_test_utils:init_per_testcase(TestCase, Config)].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config),
    teardown_mocks().

%%==============================================================================
%% Testcases
%%==============================================================================

-spec sample_job(config()) -> ok.
sample_job(_Config) ->
    {ok, Pid} = new_background_job(),
    wait_for_completion(Pid),
    ?assertEqual(length(entries()), meck:num_calls(sample_job, task, '_')),
    ?assertEqual(1, meck:num_calls(sample_job, on_complete, '_')),
    ?assertEqual(0, meck:num_calls(sample_job, on_error, '_')),
    ok.

-spec failing_job(config()) -> ok.
failing_job(_Config) ->
    {ok, Pid} = new_background_job(),
    wait_for_completion(Pid),
    ?assertEqual(1, meck:num_calls(sample_job, task, '_')),
    ?assertEqual(0, meck:num_calls(sample_job, on_complete, '_')),
    ?assertEqual(1, meck:num_calls(sample_job, on_error, '_')),
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
            timer:sleep(10),
            wait_for_completion(Pid)
    end.

-spec setup_mocks(fun((_, _) -> ok)) -> ok.
setup_mocks(Task) ->
    meck:new(sample_job, [non_strict, no_link]),
    meck:expect(sample_job, task, Task),
    meck:expect(sample_job, on_complete, fun(_) -> ok end),
    meck:expect(sample_job, on_error, fun(_) -> ok end),
    ok.

-spec teardown_mocks() -> ok.
teardown_mocks() ->
    meck:unload(sample_job),
    ok.

-spec new_background_job() -> {ok, pid()}.
new_background_job() ->
    Config = #{
        task => fun sample_job:task/2,
        entries => entries(),
        on_complete => fun sample_job:on_complete/1,
        on_error => fun sample_job:on_error/1,
        title => <<"Sample job">>
    },
    els_background_job:new(Config).

-spec entries() -> [any()].
entries() ->
    lists:seq(1, 27).
