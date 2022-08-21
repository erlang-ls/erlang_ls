%%==============================================================================
%% The Erlang Language Server
%%==============================================================================
-module(els_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

-export([start_link/0]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

%% API
-export([
    process_requests/1,
    set_io_device/1,
    send_notification/2,
    send_request/2,
    send_response/2
]).

%% Testing
-export([reset_state/0]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").
-include_lib("els_core/include/els_core.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() ::
    #{
        status := started | initialized | shutdown | exiting,
        io_device := pid() | standard_io,
        request_id := number(),
        pending := [{number(), pid()}],
        open_buffers := sets:set(buffer()),
        in_progress := [progress_entry()],
        in_progress_diagnostics := [diagnostic_entry()]
    }.
-type buffer() :: uri().
-type progress_entry() :: {uri(), pid()}.
-type diagnostic_entry() :: #{
    uri := uri(),
    pending := [pid()],
    diagnostics := [els_diagnostics:diagnostic()]
}.
-export_type([diagnostic_entry/0, state/0]).

%%==============================================================================
%% API
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, [], []),
    Cb = fun(Requests) ->
        gen_server:cast(Pid, {process_requests, Requests})
    end,
    {ok, _} = els_stdio:start_listener(Cb),
    {ok, Pid}.

-spec process_requests([any()]) -> ok.
process_requests(Requests) ->
    gen_server:cast(?SERVER, {process_requests, Requests}).

-spec set_io_device(atom() | pid()) -> ok.
set_io_device(IoDevice) ->
    gen_server:call(?SERVER, {set_io_device, IoDevice}).

-spec send_notification(binary(), map()) -> ok.
send_notification(Method, Params) ->
    gen_server:cast(?SERVER, {notification, Method, Params}).

-spec send_request(binary(), map()) -> ok.
send_request(Method, Params) ->
    gen_server:cast(?SERVER, {request, Method, Params}).

-spec send_response(pid(), any()) -> ok.
send_response(Job, Result) ->
    gen_server:cast(?SERVER, {response, Job, Result}).

%%==============================================================================
%% Testing
%%==============================================================================
-spec reset_state() -> ok.
reset_state() ->
    gen_server:call(?MODULE, {reset_state}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init([]) -> {ok, state()}.
init([]) ->
    %% Ensure the terminate function is called on shutdown, allowing the
    %% job to clean up.
    process_flag(trap_exit, true),
    ?LOG_INFO("Starting els_server..."),
    State = #{
        status => started,
        io_device => standard_io,
        request_id => 0,
        pending => [],
        open_buffers => sets:new(),
        in_progress => [],
        in_progress_diagnostics => []
    },
    {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({set_io_device, IoDevice}, _From, State) ->
    {reply, ok, State#{io_device := IoDevice}};
handle_call({reset_state}, _From, State) ->
    {reply, ok, State#{
        status => started,
        open_buffers => sets:new(),
        in_progress => [],
        in_progress_diagnostics => []
    }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({process_requests, Requests}, State0) ->
    State = lists:foldl(fun handle_request/2, State0, Requests),
    {noreply, State};
handle_cast({notification, Method, Params}, State) ->
    do_send_notification(Method, Params, State),
    {noreply, State};
handle_cast({request, Method, Params}, State0) ->
    State = do_send_request(Method, Params, State0),
    {noreply, State};
handle_cast({response, Job, Result}, State0) ->
    State = do_send_response(Job, Result, State0),
    {noreply, State};
handle_cast(_, State) ->
    {noreply, State}.

-spec handle_info(any(), els_server:state()) -> {noreply, els_server:state()}.
handle_info({result, Result, Job}, State0) ->
    ?LOG_DEBUG("Received result [job=~p]", [Job]),
    #{in_progress := InProgress} = State0,
    State = do_send_response(Job, Result, State0),
    NewState = State#{in_progress => lists:keydelete(Job, 2, InProgress)},
    {noreply, NewState};
%% LSP 3.15 introduce versioning for diagnostics. Until all clients
%% support it, we need to keep track of old diagnostics and re-publish
%% them every time we get a new chunk.
handle_info({diagnostics, Diagnostics, Job}, State) ->
    #{in_progress_diagnostics := InProgress} = State,
    ?LOG_DEBUG("Received diagnostics [job=~p]", [Job]),
    case find_entry(Job, InProgress) of
        {ok, {
            #{
                pending := Jobs,
                diagnostics := OldDiagnostics,
                uri := Uri
            },
            Rest
        }} ->
            NewDiagnostics = Diagnostics ++ OldDiagnostics,
            els_diagnostics_provider:publish(Uri, NewDiagnostics),
            NewState =
                case lists:delete(Job, Jobs) of
                    [] ->
                        State#{in_progress_diagnostics => Rest};
                    Remaining ->
                        State#{
                            in_progress_diagnostics =>
                                [
                                    #{
                                        pending => Remaining,
                                        diagnostics => NewDiagnostics,
                                        uri => Uri
                                    }
                                    | Rest
                                ]
                        }
                end,
            {noreply, NewState};
        {error, not_found} ->
            {noreply, State}
    end;
handle_info(_Request, State) ->
    {noreply, State}.

-spec terminate(any(), els_server:state()) -> ok.
terminate(_Reason, #{in_progress := InProgress}) ->
    [els_background_job:stop(Job) || {_Uri, Job} <- InProgress],
    ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(map(), state()) -> state().
handle_request(
    #{
        <<"method">> := <<"$/cancelRequest">>,
        <<"params">> := Params
    },
    State0
) ->
    #{<<"id">> := Id} = Params,
    #{pending := Pending, in_progress := InProgress} = State0,
    case lists:keyfind(Id, 1, Pending) of
        false ->
            ?LOG_DEBUG(
                "Trying to cancel not existing request [params=~p]",
                [Params]
            ),
            State0;
        {RequestId, Job} when RequestId =:= Id ->
            ?LOG_DEBUG("[SERVER] Cancelling request [id=~p] [job=~p]", [Id, Job]),
            els_background_job:stop(Job),
            Error = #{
                code => ?ERR_REQUEST_CANCELLED,
                message => <<"Request was cancelled">>
            },
            ErrorResponse = els_protocol:error(RequestId, Error),
            ?LOG_DEBUG(
                "[SERVER] Sending error response [response=~p]",
                [ErrorResponse]
            ),
            send(ErrorResponse, State0),
            State0#{
                pending => lists:keydelete(Id, 1, Pending),
                in_progress => lists:keydelete(Job, 2, InProgress)
            }
    end;
handle_request(
    #{<<"method">> := _ReqMethod} = Request,
    #{
        pending := Pending,
        in_progress := InProgress,
        in_progress_diagnostics := InProgressDiagnostics
    } = State0
) ->
    Method = maps:get(<<"method">>, Request),
    Params = maps:get(<<"params">>, Request, #{}),
    Type =
        case maps:is_key(<<"id">>, Request) of
            true -> request;
            false -> notification
        end,
    case els_methods:dispatch(Method, Params, Type, State0) of
        {response, Result, State} ->
            RequestId = maps:get(<<"id">>, Request),
            Response = els_protocol:response(RequestId, Result),
            ?LOG_DEBUG("[SERVER] Sending response [response=~s]", [Response]),
            send(Response, State0),
            State;
        {error, Error, State} ->
            RequestId = maps:get(<<"id">>, Request, null),
            ErrorResponse = els_protocol:error(RequestId, Error),
            ?LOG_DEBUG(
                "[SERVER] Sending error response [response=~s]",
                [ErrorResponse]
            ),
            send(ErrorResponse, State0),
            State;
        {noresponse, State} ->
            ?LOG_DEBUG("[SERVER] No response", []),
            State;
        {async, Uri, BackgroundJob, State} ->
            RequestId = maps:get(<<"id">>, Request),
            ?LOG_DEBUG(
                "[SERVER] Suspending response [background_job=~p id=~p]",
                [BackgroundJob, RequestId]
            ),
            NewPending = [{RequestId, BackgroundJob} | Pending],
            State#{
                pending => NewPending,
                in_progress => [{Uri, BackgroundJob} | InProgress]
            };
        {diagnostics, Uri, Jobs, State} ->
            Entry = #{uri => Uri, pending => Jobs, diagnostics => []},
            State#{in_progress_diagnostics => [Entry | InProgressDiagnostics]};
        {notification, M, P, State} ->
            do_send_notification(M, P, State0),
            State
    end;
handle_request(Response, State0) ->
    ?LOG_DEBUG(
        "[SERVER] got request response [response=~p]",
        [Response]
    ),
    State0.

-spec do_send_notification(binary(), map(), state()) -> ok.
%% This notification is specifically filtered out to avoid recursive
%% calling of log notifications (see issue #1050)
do_send_notification(<<"window/logMessage">> = Method, Params, State) ->
    Notification = els_protocol:notification(Method, Params),
    send(Notification, State);
do_send_notification(Method, Params, State) ->
    Notification = els_protocol:notification(Method, Params),
    ?LOG_DEBUG(
        "[SERVER] Sending notification [notification=~s]",
        [Notification]
    ),
    send(Notification, State).

-spec do_send_request(binary(), map(), state()) -> state().
do_send_request(Method, Params, #{request_id := RequestId0} = State0) ->
    RequestId = RequestId0 + 1,
    Request = els_protocol:request(RequestId, Method, Params),
    ?LOG_DEBUG(
        "[SERVER] Sending request [request=~p]",
        [Request]
    ),
    send(Request, State0),
    State0#{request_id => RequestId}.

-spec do_send_response(pid(), any(), state()) -> state().
do_send_response(Job, Result, State0) ->
    #{pending := Pending0} = State0,
    case lists:keyfind(Job, 2, Pending0) of
        false ->
            ?LOG_DEBUG(
                "[SERVER] Sending delayed response, but no request found [job=~p]",
                [Job]
            ),
            State0;
        {RequestId, J} when J =:= Job ->
            Response = els_protocol:response(RequestId, Result),
            ?LOG_DEBUG(
                "[SERVER] Sending delayed response [job=~p] [response=~p]",
                [Job, Response]
            ),
            send(Response, State0),
            Pending = lists:keydelete(RequestId, 1, Pending0),
            State0#{pending => Pending}
    end.

-spec send(binary(), state()) -> ok.
send(Payload, #{io_device := IoDevice}) ->
    els_stdio:send(IoDevice, Payload).

-spec find_entry(pid(), [els_server:diagnostic_entry()]) ->
    {ok, {els_server:diagnostic_entry(), [els_server:diagnostic_entry()]}}
    | {error, not_found}.
find_entry(Job, InProgress) ->
    find_entry(Job, InProgress, []).

-spec find_entry(pid(), [els_server:diagnostic_entry()], [els_server:diagnostic_entry()]) ->
    {ok, {els_server:diagnostic_entry(), [els_server:diagnostic_entry()]}}
    | {error, not_found}.
find_entry(_Job, [], []) ->
    {error, not_found};
find_entry(Job, [#{pending := Pending} = Entry | Rest], Acc) ->
    case lists:member(Job, Pending) of
        true ->
            {ok, {Entry, Rest ++ Acc}};
        false ->
            find_entry(Job, Rest, [Entry | Acc])
    end.
