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
  {ok, proc_lib:spawn_link(?MODULE, init, [{}])}.

-spec init(any()) -> no_return().
init(_Args) ->
  lager:info("Starting stdio server..."),
  ok = io:setopts(?STDIO, [binary]),
  ok = erlang_ls_server:set_connection(?STDIO),
  loop([]).

-spec send(any(), binary()) -> ok.
send(Connection, Payload) ->
  io:format(Connection, Payload, []).

%%==============================================================================
%% loop
%%==============================================================================

-spec loop([binary()]) -> no_return().
loop(Lines) ->
  case io:get_line(?STDIO) of
    <<"\n">> ->
      Headers       = parse_headers(Lines),
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      %% Use file:read/2 since it reads bytes
      {ok, Payload} = file:read(?STDIO, Length),
      Request       = jsx:decode(Payload, [return_maps]),
      erlang_ls_server:process_requests([Request]),
      loop([]);
    Line ->
      loop([Line | Lines])
  end.

-spec parse_headers([binary()]) -> [{binary(), binary()}].
parse_headers(Lines) ->
  [parse_header(Line) || Line <- Lines].

-spec parse_header(binary()) -> {binary(), binary()}.
parse_header(Line) ->
  [Name, Value] = binary:split(Line, <<":">>),
  {string:trim(string:lowercase(Name)), string:trim(Value)}.
