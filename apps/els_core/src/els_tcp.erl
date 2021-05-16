-module(els_tcp).

-behaviour(ranch_protocol).
-behaviour(els_transport).

%% ranch callbacks
-export([start_link/3]).

%% els_transport callbacks
-export([ start_listener/1
        , init/1
        , send/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

-record(state, {buffer :: binary()}).
-record(connection, {socket :: any(), transport :: module()}).

%%==============================================================================
%% Defines
%%==============================================================================

%%==============================================================================
%% Types
%%==============================================================================

-type state()      :: #state{}.
-type connection() :: #connection{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================

-spec start_link(ranch:ref(), module(), any()) -> {ok, pid()}.
start_link(Ref, Transport, Opts) ->
  Args = {Ref, Transport, Opts},
  {ok, proc_lib:spawn_link(?MODULE, init, [Args])}.

%%==============================================================================
%% els_transport callbacks
%%==============================================================================

-spec start_listener(function()) -> {ok, pid()}.
start_listener(_Cb) ->
  ?LOG_INFO("Starting ranch listener.."),
  {ok, Port} = application:get_env(els_core, port),
  {ok, _} = ranch:start_listener( els_core
                                , ranch_tcp
                                , #{socket_opts => [{port, Port}]}
                                , ?MODULE
                                , []
                                ).

-spec init({ranch:ref(), module(), any()}) -> ok.
init({Ref, Transport, _Opts}) ->
  {ok, Socket} = ranch:handshake(Ref),
  ok         = Transport:setopts(Socket, [{active, once}, {packet, 0}]),

  Connection = #connection{socket = Socket, transport = Transport},
  {ok, Server} = application:get_env(els_core, server),
  ok         = Server:set_connection(Connection),

  loop(#state{buffer = <<>>}).

-spec send(connection(), binary()) -> ok.
send(#connection{socket = Socket, transport = Transport}, Payload) ->
  Transport:send(Socket, Payload).

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================

-spec loop(state()) -> ok.
loop(#state{buffer = Buffer} = State) ->
  receive
    {tcp, Socket, Packet} ->
      Data = <<Buffer/binary, Packet/binary>>,
      {Requests, NewBuffer} = els_jsonrpc:split(Data, [return_maps]),
      ok = els_server:process_requests(Requests),
      inet:setopts(Socket, [{active, once}]),
      loop(State#state{buffer = NewBuffer});
    {tcp_closed, _Socket} ->
      ok = els_server:process_requests([#{
                                          <<"method">> => <<"exit">>,
                                          <<"params">> => []
                                         }]),
      ok;
    Message ->
      ?LOG_WARNING("Unsupported message: ~p", [Message]),
      loop(State)
  end.
