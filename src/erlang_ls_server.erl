%%==============================================================================
%% The Erlang Language Server
%%==============================================================================
-module(erlang_ls_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(ranch_protocol).
-behaviour(gen_statem).

%%==============================================================================
%% Exports
%%==============================================================================
%% ranch_protocol callbacks
-export([ start_link/4 ]).

%% gen_statem callbacks
-export([ callback_mode/0
        , code_change/4
        , init/1
        , terminate/3
        ]).

%% gen_statem state functions
-export([ connected/3
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { socket :: port()
               , buffer :: binary()
               , state  :: map()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================
-spec start_link(ranch:ref(), any(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%==============================================================================
%% gen_statem callbacks
%%==============================================================================
-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

-spec init({ranch:ref(), any(), module(), any()}) -> no_return().
init({Ref, Socket, Transport, _Opts}) ->
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [ {active, once}
                                 , {packet, 0}
                                 ]),
  gen_statem:enter_loop( ?MODULE
                       , []
                       , connected
                       , #state{ socket = Socket
                               , buffer = <<>>
                               , state  = #{}
                               }
                       ).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec terminate(any(), atom(), state()) -> any().
terminate(_Reason, _StateName, #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================
-spec connected(gen_statem:event_type(), any(), state()) -> any().
connected(info, {tcp, Socket, Packet}, #state{buffer = Buffer} = State0) ->
  lager:debug("[SERVER] TCP Packet [buffer=~p] [packet=~p] ", [Buffer, Packet]),
  Data = <<Buffer/binary, Packet/binary>>,
  {Requests, NewBuffer} = erlang_ls_jsonrpc:split(Data, [return_maps]),
  State   = lists:foldl(fun handle_request/2, State0, Requests),
  inet:setopts(Socket, [{active, once}]),
  {keep_state, State#state{ buffer = NewBuffer }};
connected(info, {tcp_closed, _Socket}, _State) ->
  {stop, normal};
connected(info, {'EXIT', _, normal}, _State) ->
  keep_state_and_data;
connected(info, {tcp_error, _, Reason}, _State) ->
  {stop, Reason};
connected(cast, {notification, M, P}, State) ->
  send_notification(State#state.socket, M, P),
  keep_state_and_data.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(any(), state()) -> state().
handle_request(Request, #state{ socket = Socket
                              , state  = InternalState
                              } = State0) ->
  Method = maps:get(<<"method">>, Request),
  Params = maps:get(<<"params">>, Request),
  case erlang_ls_methods:dispatch(Method, Params, InternalState) of
    {response, Result, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = erlang_ls_protocol:response(RequestId, Result),
      lager:debug("[SERVER] Sending response [response=~p]", [Response]),
      gen_tcp:send(Socket, Response),
      State0#state{state = NewInternalState};
    {error, Error, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request, null),
      ErrorResponse = erlang_ls_protocol:error(RequestId, Error),
      lager:debug( "[SERVER] Sending error response [response=~p]"
                 , [ErrorResponse]
                 ),
      gen_tcp:send(Socket, ErrorResponse),
      State0#state{state = NewInternalState};
    {noresponse, NewInternalState} ->
      lager:debug("[SERVER] No response", []),
      State0#state{state = NewInternalState};
    {notification, M, P, NewInternalState} ->
      send_notification(Socket, M, P),
      State0#state{state = NewInternalState}
  end.

-spec send_notification(any(), binary(), map()) -> ok.
send_notification(Socket, Method, Params) ->
  Notification = erlang_ls_protocol:notification(Method, Params),
  lager:debug("[SERVER] Sending notification [notification=~p]", [Notification]),
  gen_tcp:send(Socket, Notification).
