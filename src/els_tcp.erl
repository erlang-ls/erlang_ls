-module(els_tcp).

-behaviour(ranch_protocol).
-behaviour(els_transport).

%% ranch callbacks
-export([start_link/4]).

%% els_transport callbacks
-export([ start_listener/1
        , init/1
        , send/2
        ]).

-record(state, { buffer :: binary()
               , server :: pid()
               }).
-record(connection, {socket :: any(), transport :: module()}).

%%==============================================================================
%% Defines
%%==============================================================================

-define(DEFAULT_PORT, 10000).

%%==============================================================================
%% Types
%%==============================================================================

-type state()      :: #state{}.
-type connection() :: #connection{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================

-spec start_link(ranch:ref(), any(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  Args = {Ref, Socket, Transport, Opts},
  {ok, proc_lib:spawn_link(?MODULE, init, [Args])}.

%%==============================================================================
%% els_transport callbacks
%%==============================================================================

-spec start_listener(pid()) -> {ok, pid()}.
start_listener(Server) ->
  lager:info("Starting ranch listener.."),
  {ok, Port} = application:get_env(erlang_ls, port),
  {ok, _} = ranch:start_listener( erlang_ls
                                , ranch_tcp
                                , #{socket_opts => [{port, Port}]}
                                , ?MODULE
                                , [{server, Server}]
                                ).

-spec init({ranch:ref(), any(), module(), any()}) -> ok.
init({Ref, Socket, Transport, Opts}) ->
  Server     = proplists:get_value(server, Opts),
  {ok, _}    = ranch:handshake(Ref),
  ok         = Transport:setopts(Socket, [{active, once}, {packet, 0}]),

  Connection = #connection{socket = Socket, transport = Transport},
  ok         = els_server:set_connection(Server, Connection),

  loop(#state{buffer = <<>>, server = Server}).

-spec send(connection(), binary()) -> ok.
send(#connection{socket = Socket, transport = Transport}, Payload) ->
  Transport:send(Socket, Payload).

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================

-spec loop(state()) -> ok.
loop(#state{buffer = Buffer, server = Server} = State) ->
  receive
    {tcp, Socket, Packet} ->
      Data = <<Buffer/binary, Packet/binary>>,
      {Requests, NewBuffer} = els_jsonrpc:split(Data, [return_maps]),
      ok = els_server:process_requests(Server, Requests),
      inet:setopts(Socket, [{active, once}]),
      loop(State#state{buffer = NewBuffer});
    {tcp_closed, _Socket} ->
      ok;
    Message ->
      lager:warning("Unsupported message: ~p", [Message]),
      loop(State)
  end.
