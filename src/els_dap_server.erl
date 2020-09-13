%%==============================================================================
%% The Erlang DAP Server
%%==============================================================================
-module(els_dap_server).

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
        , send_event/2
        , send_request/2
        ]).

%% Testing
-export([ reset_internal_state/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { transport      :: module()
               , connection     :: any()
               , seq            :: number()
               , internal_state :: map()
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

-spec send_event(binary(), map()) -> ok.
send_event(EventType, Body) ->
  gen_server:cast(?SERVER, {event, EventType, Body}).

-spec send_request(binary(), map()) -> ok.
send_request(Method, Params) ->
  gen_server:cast(?SERVER, {request, Method, Params}).

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
  lager:info("Starting els_dap_server..."),
  State = #state{ transport      = Transport
                , seq            = 0
                , internal_state = #{}
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
handle_cast({event, EventType, Body}, State0) ->
  State = do_send_event(EventType, Body, State0),
  {noreply, State};
handle_cast({request, Method, Params}, State0) ->
  State = do_send_request(Method, Params, State0),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(map(), state()) -> state().
handle_request(#{ <<"seq">> := Seq
                , <<"type">> := <<"request">>
                , <<"command">> := Command
                } = Request, #state{internal_state = InternalState} = State0) ->
  Args = maps:get(<<"arguments">>, Request, #{}),
  case els_dap_methods:dispatch(Command, Args, request, InternalState) of
    {response, Result, NewInternalState} ->
      Response = els_dap_protocol:response(Seq, Command, Result),
      lager:debug("[SERVER] Sending response [response=~s]", [Response]),
      send(Response, State0),
      State0#state{internal_state = NewInternalState};
    {error_response, Error, NewInternalState} ->
      Response = els_dap_protocol:error_response(Seq, Command, Error),
      lager:debug("[SERVER] Sending error response [response=~s]", [Response]),
      send(Response, State0),
      State0#state{internal_state = NewInternalState}
  end;
handle_request(Response, State0) ->
  lager:debug( "[SERVER] got request response [response=~p]"
             , [Response]
             ),
  State0.

-spec do_send_event(binary(), map(), state()) -> state().
do_send_event(EventType, Body, #state{seq = Seq0} = State0) ->
  Seq = Seq0 + 1,
  Event = els_dap_protocol:event(Seq, EventType, Body),
  lager:debug( "[SERVER] Sending event [type=~s]", [EventType]),
  send(Event, State0),
  State0#state{seq = Seq}.

-spec do_send_request(binary(), map(), state()) -> state().
do_send_request(Method, Params, #state{seq = RequestId0} = State0) ->
  RequestId = RequestId0 + 1,
  Request = els_protocol:request(RequestId, Method, Params),
  lager:debug( "[SERVER] Sending request [request=~p]"
             , [Request]
             ),
  send(Request, State0),
  State0#state{seq = RequestId}.

-spec send(binary(), state()) -> ok.
send(Payload, #state{transport = T, connection = C}) ->
  T:send(C, Payload).
