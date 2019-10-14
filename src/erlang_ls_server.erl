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
-record(state, {socket, document}).

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
                               , document = <<>>
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
connected(info, {tcp, Socket, Packet}, #state{ socket = Socket
                                             , document = Document
                                             } = State) ->
  lager:debug("[SERVER] TCP Packet [document=~p] [packet=~p] ", [Document, Packet]),
  Data = <<Document/binary, Packet/binary>>,
  {Requests, NewDocument} = erlang_ls_jsonrpc:split(Data, [return_maps]),
  [handle_request(Socket, Request) || Request <- Requests],
  inet:setopts(Socket, [{active, once}]),
  {keep_state, State#state{ document = NewDocument }};
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
-spec handle_request(any(), map()) -> ok.
handle_request(Socket, Request) ->
  Method    = maps:get(<<"method">>, Request),
  Params    = maps:get(<<"params">>, Request),
  case handle_method(Method, Params) of
    {response, Result} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = erlang_ls_protocol:response(RequestId, Result),
      lager:debug("[SERVER] Sending response [response=~p]", [Response]),
      gen_tcp:send(Socket, Response);
    {} ->
      lager:debug("[SERVER] No response", []),
      ok;
    {notification, M, P} ->
      send_notification(Socket, M, P)
  end.

%% @doc Dispatch the handling of the method to erlang_ls_protocol_impl
-spec handle_method(binary(), map()) ->
  {response, map() | null} | {} | {notification, binary(), map()}.
handle_method(Method, Params) ->
  Function = method_to_function_name(Method),
  try erlang_ls_protocol_impl:Function(Params)
  catch error:undef -> not_implemented_method(Method)
  end.

-spec not_implemented_method(binary()) ->
  {response, map()} | {} | {notification, binary(), map()}.
not_implemented_method(Method) ->
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  Message = <<"Method not implemented: ", Method/binary>>,
  Method1 = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  {notification, Method1, Params}.

-spec send_notification(any(), binary(), map()) -> ok.
send_notification(Socket, Method, Params) ->
  Notification = erlang_ls_protocol:notification(Method, Params),
  lager:debug("[SERVER] Sending notification [notification=~p]", [Notification]),
  gen_tcp:send(Socket, Notification).

-spec method_to_function_name(binary()) -> atom().
method_to_function_name(Method) ->
  Replaced = string:replace(Method, <<"/">>, <<"_">>),
  Lower    = string:lowercase(Replaced),
  Binary   = erlang:iolist_to_binary(Lower),
  binary_to_atom(Binary, utf8).
