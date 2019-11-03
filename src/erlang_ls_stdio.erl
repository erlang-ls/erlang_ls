-module(erlang_ls_stdio).

-behaviour(erlang_ls_transport).

-export([ start_listener/1
        , init/1
        , send/2
        ]).

-export([loop/4]).

-define(STDIO, standard_io).

%%==============================================================================
%% erlang_ls_transport callbacks
%%==============================================================================
-spec start_listener(pid()) -> {ok, pid()}.
start_listener(Server) ->
  IoDevice = application:get_env(erlang_ls, io_device, ?STDIO),
  {ok, proc_lib:spawn_link(?MODULE, init, [{Server, IoDevice}])}.

-spec init({pid(), any()}) -> no_return().
init({Server, IoDevice}) ->
  lager:info("Starting stdio server..."),
  ok = io:setopts(IoDevice, [binary]),
  ok = erlang_ls_server:set_connection(Server, IoDevice),
  ?MODULE:loop([], IoDevice, Server, [return_maps]).

-spec send(any(), binary()) -> ok.
send(Connection, Payload) ->
  io:format(Connection, Payload, []).

%%==============================================================================
%% Listener loop function
%%==============================================================================

-spec loop([binary()], any(), pid(), [any()]) -> no_return().
loop(Lines, IoDevice, Server, JsonOpts) ->
  case io:get_line(IoDevice, "") of
    <<"\n">> ->
      Headers       = parse_headers(Lines),
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      %% Use file:read/2 since it reads bytes
      {ok, Payload} = file:read(IoDevice, Length),
      Request       = jsx:decode(Payload, JsonOpts),
      erlang_ls_server:process_requests(Server, [Request]),
      ?MODULE:loop([], IoDevice, Server, JsonOpts);
    Line ->
      ?MODULE:loop([Line | Lines], IoDevice, Server, JsonOpts)
  end.

-spec parse_headers([binary()]) -> [{binary(), binary()}].
parse_headers(Lines) ->
  [parse_header(Line) || Line <- Lines].

-spec parse_header(binary()) -> {binary(), binary()}.
parse_header(Line) ->
  [Name, Value] = binary:split(Line, <<":">>),
  {string:trim(string:lowercase(Name)), string:trim(Value)}.
