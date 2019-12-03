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
               , buffer         :: binary()
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
  {ok, _} = Transport:start_listener(Pid),
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

%%==============================================================================
%% Testing
%%==============================================================================
-spec reset_internal_state() -> ok.
reset_internal_state() ->
  gen_server:call(?MODULE, {reset_internal_state}).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================
-spec init(module()) -> no_return().
init(Transport) ->
  lager:info("Starting els_server..."),
  State = #state{ transport      = Transport
                , buffer         = <<>>
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
handle_cast({notification, Method, Params}, State) ->
  do_send_notification(Method, Params, State),
  {noreply, State};
handle_cast(_, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(map(), state()) -> state().
handle_request(Request, #state{internal_state = InternalState} = State0) ->
  Method = maps:get(<<"method">>, Request),
  Params = maps:get(<<"params">>, Request),
  case els_methods:dispatch(Method, Params, InternalState) of
    {response, Result, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = els_protocol:response(RequestId, Result),
      lager:debug("[SERVER] Sending response [response=~p]", [Response]),
      send(Response, State0),
      State0#state{internal_state = NewInternalState};
    {error, Error, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request, null),
      ErrorResponse = els_protocol:error(RequestId, Error),
      lager:debug( "[SERVER] Sending error response [response=~p]"
                 , [ErrorResponse]
                 ),
      send(ErrorResponse, State0),
      State0#state{internal_state = NewInternalState};
    {noresponse, NewInternalState} ->
      lager:debug("[SERVER] No response", []),
      State0#state{internal_state = NewInternalState};
    {notification, M, P, NewInternalState} ->
      do_send_notification(M, P, State0),
      State0#state{internal_state = NewInternalState}
  end.

-spec do_send_notification(binary(), map(), state()) -> ok.
do_send_notification(Method, Params, State) ->
  Notification = els_protocol:notification(Method, Params),
  lager:debug( "[SERVER] Sending notification [notification=~p]"
             , [Notification]
             ),
  send(Notification, State).

-spec send(iolist(), state()) -> ok.
send(Payload, #state{transport = T, connection = C}) ->
  T:send(C, Payload).
