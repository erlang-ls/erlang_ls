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

-export([ start_link/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%% API
-export([ process_requests/1
        , set_connection/1
        , send_notification/2
        , send_request/2
        , send_response/2
        ]).

%% Testing
-export([ reset_internal_state/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { transport      :: module()
               , connection     :: any()
               , request_id     :: number()
               , internal_state :: map()
               , pending        :: [{number(), els_provider:provider(), pid()}]
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% API
%%==============================================================================
-spec start_link(module()) -> {ok, pid()}.
start_link(Transport) ->
  {ok, Pid} = gen_server:start_link({local, ?SERVER}, ?MODULE, Transport, []),
  Cb = fun(Requests) ->
           gen_server:cast(Pid, {process_requests, Requests})
       end,
  {ok, _} = Transport:start_listener(Cb),
  {ok, Pid}.

-spec process_requests([any()]) -> ok.
process_requests(Requests) ->
  gen_server:cast(?SERVER, {process_requests, Requests}).

-spec set_connection(any()) -> ok.
set_connection(Connection) ->
  gen_server:call(?SERVER, {set_connection, Connection}).

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
-spec reset_internal_state() -> ok.
reset_internal_state() ->
  gen_server:call(?MODULE, {reset_internal_state}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(module()) -> {ok, state()}.
init(Transport) ->
  ?LOG_INFO("Starting els_server..."),
  State = #state{ transport      = Transport
                , request_id     = 0
                , internal_state = #{}
                , pending        = []
                },
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({set_connection, Connection}, _From, State) ->
  {reply, ok, State#state{connection = Connection}};
handle_call({reset_internal_state}, _From, State) ->
  {reply, ok, State#state{internal_state = #{}}}.

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

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(map(), state()) -> state().
handle_request(#{ <<"method">> := <<"$/cancelRequest">>
                , <<"params">> := Params
                }, State0) ->
  #{<<"id">> := Id} = Params,
  #state{pending = Pending} = State0,
  case lists:keyfind(Id, 1, Pending) of
    false ->
      ?LOG_DEBUG("Trying to cancel not existing request [params=~p]",
                 [Params]),
      State0;
    {RequestId, Provider, Job} when RequestId =:= Id ->
      ?LOG_DEBUG("[SERVER] Cancelling request [id=~p] [provider=~p] [job=~p]",
                 [Id, Provider, Job]),
      els_provider:cancel_request(Provider, Job),
      State0#state{pending = lists:keydelete(Id, 1, Pending)}
  end;
handle_request(#{ <<"method">> := _ReqMethod } = Request
              , #state{ internal_state = InternalState
                      , pending = Pending
                      } = State0) ->
  Method = maps:get(<<"method">>, Request),
  Params = maps:get(<<"params">>, Request),
  Type = case maps:is_key(<<"id">>, Request) of
           true  -> request;
           false -> notification
         end,
  case els_methods:dispatch(Method, Params, Type, InternalState) of
    {response, Result, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = els_protocol:response(RequestId, Result),
      ?LOG_DEBUG("[SERVER] Sending response [response=~s]", [Response]),
      send(Response, State0),
      State0#state{internal_state = NewInternalState};
    {error, Error, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request, null),
      ErrorResponse = els_protocol:error(RequestId, Error),
      ?LOG_DEBUG( "[SERVER] Sending error response [response=~s]"
                , [ErrorResponse]
                ),
      send(ErrorResponse, State0),
      State0#state{internal_state = NewInternalState};
    {noresponse, NewInternalState} ->
      ?LOG_DEBUG("[SERVER] No response", []),
      State0#state{internal_state = NewInternalState};
    {noresponse, {Provider, BackgroundJob}, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request),
      ?LOG_DEBUG("[SERVER] Suspending response [background_job=~p]",
                 [BackgroundJob]),
      NewPending = [{RequestId, Provider, BackgroundJob}| Pending],
      State0#state{ internal_state = NewInternalState
                  , pending = NewPending
                  };
    {notification, M, P, NewInternalState} ->
      do_send_notification(M, P, State0),
      State0#state{internal_state = NewInternalState}
  end;
handle_request(Response, State0) ->
  ?LOG_DEBUG( "[SERVER] got request response [response=~p]"
            , [Response]
            ),
  State0.

-spec do_send_notification(binary(), map(), state()) -> ok.
do_send_notification(Method, Params, State) ->
  Notification = els_protocol:notification(Method, Params),
  ?LOG_DEBUG( "[SERVER] Sending notification [notification=~s]"
            , [Notification]
            ),
  send(Notification, State).

-spec do_send_request(binary(), map(), state()) -> state().
do_send_request(Method, Params, #state{request_id = RequestId0} = State0) ->
  RequestId = RequestId0 + 1,
  Request = els_protocol:request(RequestId, Method, Params),
  ?LOG_DEBUG( "[SERVER] Sending request [request=~p]"
            , [Request]
            ),
  send(Request, State0),
  State0#state{request_id = RequestId}.

-spec do_send_response(pid(), any(), state()) -> state().
do_send_response(Job, Result, State0) ->
  #state{pending = Pending0} = State0,
  case lists:keyfind(Job, 3, Pending0) of
    false ->
      ?LOG_DEBUG(
         "[SERVER] Sending delayed response, but no request found [job=~p]",
         [Job]),
      State0;
    {RequestId, _Provider, J} when J =:= Job ->
      Response = els_protocol:response(RequestId, Result),
      ?LOG_DEBUG( "[SERVER] Sending delayed response [job=~p] [response=~p]"
                , [Job, Response]
                ),
      send(Response, State0),
      Pending = lists:keydelete(RequestId, 1, Pending0),
      State0#state{pending = Pending}
  end.

-spec send(binary(), state()) -> ok.
send(Payload, #state{transport = T, connection = C}) ->
  T:send(C, Payload).
