%%==============================================================================
%% The Erlang Language Server
%%==============================================================================
-module(erlang_ls_server).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_statem).

%%==============================================================================
%% Exports
%%==============================================================================

-export([ start/1
        , start_link/2
        ]).

%% gen_statem callbacks
-export([ callback_mode/0
        , code_change/4
        , init/1
        , terminate/3
        ]).

%% gen_statem state functions
-export([ connected/3
        ]).

%% Notifications API
-export([ send_notification/3 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { transport  :: module()
               , connection :: any()
               , buffer     :: binary()
               , state      :: map()
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% Start server
%%==============================================================================

-spec start(module()) -> ok.
start(Transport) ->
  Transport:start().

-spec start_link(module(), any()) -> {ok, pid()}.
start_link(Transport, Args) ->
  {ok, proc_lib:spawn_link(erlang_ls_server, init, [{Transport, Args}])}.

%%==============================================================================
%% gen_statem callbacks
%%==============================================================================
-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

-spec init({module(), any()}) -> no_return().
init({Transport, Args}) ->
  Connection = Transport:init(Args),
  State = #state{ transport  = Transport
                , connection = Connection
                , buffer     = <<>>
                , state      = #{}
                },
  gen_statem:enter_loop(?MODULE, [], connected, State).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec terminate(any(), atom(), state()) -> any().
terminate( _Reason
         , _StateName
         , #state{ transport  = Transport, connection = Connection}
         ) ->
  Transport:close(Connection),
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
connected( cast
         , {notification, M, P}
         , #state{transport = Transport , connection = Connection}
         ) ->
  do_send_notification(Transport, Connection, M, P),
  keep_state_and_data.

%%==============================================================================
%% Notifications API
%%==============================================================================
-spec send_notification(pid(), binary(), map()) -> ok.
send_notification(Server, Method, Params) ->
  gen_server:cast(Server, {notification, Method, Params}).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(any(), state()) -> state().
handle_request(Request, #state{ transport  = Transport
                              , connection = Connection
                              , state      = InternalState
                              } = State0) ->
  Method = maps:get(<<"method">>, Request),
  Params = maps:get(<<"params">>, Request),
  case erlang_ls_methods:dispatch(Method, Params, InternalState) of
    {response, Result, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = erlang_ls_protocol:response(RequestId, Result),
      lager:debug("[SERVER] Sending response [response=~p]", [Response]),
      Transport:send(Connection, Response),
      State0#state{state = NewInternalState};
    {error, Error, NewInternalState} ->
      RequestId = maps:get(<<"id">>, Request, null),
      ErrorResponse = erlang_ls_protocol:error(RequestId, Error),
      lager:debug( "[SERVER] Sending error response [response=~p]"
                 , [ErrorResponse]
                 ),
      Transport:send(Connection, ErrorResponse),
      State0#state{state = NewInternalState};
    {noresponse, NewInternalState} ->
      lager:debug("[SERVER] No response", []),
      State0#state{state = NewInternalState};
    {notification, M, P, NewInternalState} ->
      do_send_notification(Transport, Connection, M, P),
      State0#state{state = NewInternalState}
  end.

-spec do_send_notification(module(), any(), binary(), map()) -> ok.
do_send_notification(Transport, Connection, Method, Params) ->
  Notification = erlang_ls_protocol:notification(Method, Params),
  lager:debug( "[SERVER] Sending notification [notification=~p]"
             , [Notification]),
  Transport:send(Connection, Notification).
