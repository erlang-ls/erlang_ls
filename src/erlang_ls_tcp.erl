-module(erlang_ls_tcp).

-behaviour(ranch_protocol).
-behaviour(erlang_ls_transport).

-export([start_link/4]).

-export([ start/0
        , init/1
        , recv/1
        , send/2
        , close/1
        ]).

%%==============================================================================
%% Defines
%%==============================================================================

-define(DEFAULT_PORT, 10000).

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: {any(), any()}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================

-spec start_link(ranch:ref(), any(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  Args = {Ref, Socket, Transport, Opts},
  erlang_ls_server:start_link(?MODULE, Args).

%%==============================================================================
%% els_transport callbacks
%%==============================================================================

-spec start() -> ok.
start() ->
  Port = application:get_env(erlang_ls, port, ?DEFAULT_PORT),
  {ok, _} = ranch:start_listener( erlang_ls
                                , ranch_tcp
                                , #{socket_opts => [{port, Port}]}
                                , erlang_ls_tcp
                                , []
                                ),
  ok.

-spec init({ranch:ref(), any(), module(), any()}) -> state().
init({Ref, Socket, Transport, _Opts}) ->
  {ok, _} = ranch:handshake(Ref),
  ok = Transport:setopts(Socket, [{active, once}, {packet, 0}]),
  {Socket, Transport}.

-spec recv(state()) -> binary().
recv({Socket, Transport}) ->
  case Transport:recv(Socket, 0, infinity) of
    {ok, Payload}   -> {ok, Payload};
    {error, Error} -> {error, Error}
  end.

-spec send(state(), binary()) -> ok.
send({Socket, Transport}, Payload) ->
  Transport:send(Socket, Payload).

-spec close(state()) -> ok.
close({Socket, Transport}) ->
  Transport:close(Socket).
