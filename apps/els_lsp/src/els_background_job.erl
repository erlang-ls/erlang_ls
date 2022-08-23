%%==============================================================================
%% API and Implementation for Background Jobs
%%==============================================================================
-module(els_background_job).

%%==============================================================================
%% API
%%==============================================================================
-export([
    new/1,
    list/0,
    stop/1,
    stop_all/0
]).

-export([start_link/1]).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-behaviour(gen_server).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macro Definitions
%%==============================================================================

%% ms
-define(SPINNING_WHEEL_INTERVAL, 100).

%%==============================================================================
%% Types
%%==============================================================================
-type entry() :: any().
-type config() :: #{
    task := fun((entry(), any()) -> any()),
    entries := [any()],
    on_complete => fun(),
    on_error => fun(),
    title := binary(),
    show_percentages => boolean(),
    initial_state => any()
}.
-type state() :: #{
    config := config(),
    progress_enabled := boolean(),
    show_percentages := boolean(),
    token := els_progress:token(),
    current := non_neg_integer(),
    step := pos_integer(),
    total := non_neg_integer(),
    internal_state := any(),
    spinning_wheel := pid() | undefined
}.

%%==============================================================================
%% API
%%==============================================================================

%% @doc Create a new background job
-spec new(config()) -> {ok, pid()}.
new(Config) ->
    supervisor:start_child(els_background_job_sup, [Config]).

%% @doc Return the list of running background jobs
-spec list() -> [pid()].
list() ->
    [
        Pid
     || {_Id, Pid, _Type, _Modules} <-
            supervisor:which_children(els_background_job_sup)
    ].

%% @doc Terminate a background job
-spec stop(pid()) -> ok.
stop(Pid) ->
    supervisor:terminate_child(els_background_job_sup, Pid).

%% @doc Terminate all background jobs
-spec stop_all() -> ok.
stop_all() ->
    [
        ok = supervisor:terminate_child(els_background_job_sup, Pid)
     || Pid <- list()
    ],
    ok.

%% @doc Start the server responsible for a background job
%%
%% To be used by the supervisor
-spec start_link(config()) -> {ok, pid()}.
start_link(Config) ->
    gen_server:start_link(?MODULE, Config, []).

%%==============================================================================
%% Callbacks for gen_server
%%==============================================================================
-spec init(config()) -> {ok, state()}.
init(#{entries := Entries, title := Title} = Config) ->
    ?LOG_DEBUG("Background job started ~s", [Title]),
    %% Ensure the terminate function is called on shutdown, allowing the
    %% job to clean up.
    process_flag(trap_exit, true),
    ProgressEnabled = els_work_done_progress:is_supported(),
    Total = length(Entries),
    Step = step(Total),
    Token = els_work_done_progress:send_create_request(),
    OnComplete = maps:get(on_complete, Config, fun noop/1),
    OnError = maps:get(on_error, Config, fun noop/1),
    ShowPercentages = maps:get(show_percentages, Config, true),
    notify_begin(Token, Title, Total, ProgressEnabled, ShowPercentages),
    SpinningWheel =
        case {ProgressEnabled, ShowPercentages} of
            {true, false} ->
                spawn_link(fun() -> spinning_wheel(Token) end);
            {_, _} ->
                undefined
        end,
    self() ! exec,
    {ok, #{
        config => Config#{
            on_complete => OnComplete,
            on_error => OnError
        },
        progress_enabled => ProgressEnabled,
        show_percentages => ShowPercentages,
        token => Token,
        current => 0,
        step => Step,
        total => Total,
        internal_state => maps:get(initial_state, Config, undefined),
        spinning_wheel => SpinningWheel
    }}.

-spec handle_call(any(), {pid(), any()}, state()) ->
    {noreply, state()}.
handle_call(_Request, _From, State) ->
    {noreply, State}.

-spec handle_cast(any(), any()) ->
    {noreply, state()}.
handle_cast(_Request, State) ->
    {noreply, State}.

-spec handle_info(any(), any()) ->
    {noreply, state()}.
handle_info({exec, InternalState}, State) ->
    handle_info(exec, State#{internal_state => InternalState});
handle_info(exec, State) ->
    #{
        config := #{entries := Entries, task := Task} = Config,
        progress_enabled := ProgressEnabled,
        show_percentages := ShowPercentages,
        token := Token,
        current := Current,
        step := Step,
        total := Total,
        internal_state := InternalState
    } = State,
    case Entries of
        [] ->
            notify_end(Token, Total, ProgressEnabled),
            {stop, normal, State};
        [Entry | Rest] ->
            MainPid = self(),
            %% Run the task in a separate process so main process
            %% is not blocked from receiving messages, needed for stopping
            %% job.
            spawn_link(
                fun() ->
                    NewInternalState = Task(Entry, InternalState),
                    notify_report(
                        Token,
                        Current,
                        Step,
                        Total,
                        ProgressEnabled,
                        ShowPercentages
                    ),
                    MainPid ! {exec, NewInternalState}
                end
            ),
            {noreply, State#{
                config => Config#{entries => Rest},
                current => Current + 1
            }}
    end;
%% Allow the terminate function to be called if the spawned child processes dies
handle_info({'EXIT', _Sender, Reason}, State) when Reason /= normal ->
    {stop, Reason, State};
handle_info(_Request, State) ->
    {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(normal, #{
    config := #{on_complete := OnComplete},
    internal_state := InternalState,
    spinning_wheel := SpinningWheel
}) ->
    case SpinningWheel of
        undefined ->
            ok;
        Pid ->
            exit(Pid, kill)
    end,
    ?LOG_DEBUG("Background job completed.", []),
    OnComplete(InternalState),
    ok;
terminate(Reason, #{
    config := #{
        on_error := OnError,
        title := Title
    },
    internal_state := InternalState,
    token := Token,
    total := Total,
    progress_enabled := ProgressEnabled
}) ->
    case Reason of
        shutdown ->
            ?LOG_DEBUG("Background job terminated.", []);
        _ ->
            ?LOG_ERROR(
                "Background job aborted. [reason=~p] [title=~p",
                [Reason, Title]
            )
    end,
    notify_end(Token, Total, ProgressEnabled),
    OnError(InternalState),
    ok.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec step(pos_integer()) -> pos_integer().
step(0) -> 0;
step(N) -> 100 / N.

-spec progress_msg(non_neg_integer(), pos_integer()) -> binary().
progress_msg(Current, Total) ->
    list_to_binary(io_lib:format("~p / ~p", [Current, Total])).

-spec noop(any()) -> ok.
noop(_) ->
    ok.

-spec notify_begin(
    els_progress:token(),
    binary(),
    pos_integer(),
    boolean(),
    boolean()
) ->
    ok.
notify_begin(Token, Title, Total, true, ShowPercentages) ->
    BeginMsg = progress_msg(0, Total),
    Begin =
        case ShowPercentages of
            true -> els_work_done_progress:value_begin(Title, BeginMsg, 0);
            false -> els_work_done_progress:value_begin(Title, BeginMsg)
        end,
    els_progress:send_notification(Token, Begin);
notify_begin(_Token, _Title, _Total, false, _ShowPercentages) ->
    ok.

-spec notify_report(
    els_progress:token(),
    pos_integer(),
    pos_integer(),
    pos_integer(),
    boolean(),
    boolean()
) -> ok.
notify_report(Token, Current, Step, Total, true, true) ->
    Percentage = floor(Current * Step),
    ReportMsg = progress_msg(Current, Total),
    Report = els_work_done_progress:value_report(ReportMsg, Percentage),
    els_progress:send_notification(Token, Report);
notify_report(
    _Token,
    _Current,
    _Step,
    _Total,
    _ProgressEnabled,
    _ShowPercentages
) ->
    ok.

-spec notify_end(els_progress:token(), pos_integer(), boolean()) -> ok.
notify_end(Token, Total, true) ->
    EndMsg = progress_msg(Total, Total),
    End = els_work_done_progress:value_end(EndMsg),
    els_progress:send_notification(Token, End);
notify_end(_Token, _Total, false) ->
    ok.

-spec spinning_wheel(els_progress:token()) -> no_return().
spinning_wheel(Token) ->
    Report = els_work_done_progress:value_report(<<>>),
    els_progress:send_notification(Token, Report),
    timer:sleep(?SPINNING_WHEEL_INTERVAL),
    spinning_wheel(Token).
