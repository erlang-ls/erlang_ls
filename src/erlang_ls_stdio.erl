-module(erlang_ls_stdio).

-behaviour(erlang_ls_transport).

-export([ start_listener/0
        , init/1
        , send/2
        ]).

-define(STDIO, standard_io).

%%==============================================================================
%% erlang_ls_transport callbacks
%%==============================================================================
-spec start_listener() -> {ok, pid()}.
start_listener() ->
  IoDevice = application:get_env(erlang_ls, io_device, ?STDIO),
  {ok, proc_lib:spawn_link(?MODULE, init, [IoDevice])}.

-spec init(any()) -> no_return().
init(IoDevice) ->
  lager:info("Starting stdio server..."),
  ok = io:setopts(IoDevice, [binary]),
  ok = erlang_ls_server:set_connection(IoDevice),
  loop([], IoDevice).

-spec send(any(), binary()) -> ok.
send(Connection, Payload) ->
  io:format(Connection, Payload, []).

%%==============================================================================
%% loop
%%==============================================================================

-spec loop([binary()], any()) -> no_return().
loop(Lines, IoDevice) ->
  case io:get_line(?STDIO) of
    <<"\n">> ->
      Headers       = parse_headers(Lines),
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      %% Use file:read/2 since it reads bytes
      {ok, Payload} = file:read(IoDevice, Length),
      Request       = jsx:decode(Payload, [return_maps]),
      erlang_ls_server:process_requests([Request]),
      loop([], IoDevice);
    Line ->
      loop([Line | Lines], IoDevice)
  end.

-spec parse_headers([binary()]) -> [{binary(), binary()}].
parse_headers(Lines) ->
  [parse_header(Line) || Line <- Lines].

-spec parse_header(binary()) -> {binary(), binary()}.
parse_header(Line) ->
  [Name, Value] = binary:split(Line, <<":">>),
  {string:trim(string:lowercase(Name)), string:trim(Value)}.
