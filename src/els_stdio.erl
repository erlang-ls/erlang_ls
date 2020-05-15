-module(els_stdio).

-behaviour(els_transport).

-export([ start_listener/1
        , init/1
        , send/2
        ]).

-export([ loop/4 ]).

%%==============================================================================
%% els_transport callbacks
%%==============================================================================
-spec start_listener(function()) -> {ok, pid()}.
start_listener(Cb) ->
  {ok, IoDevice} = application:get_env(erlang_ls, io_device),
  {ok, proc_lib:spawn_link(?MODULE, init, [{Cb, IoDevice}])}.

-spec init({function(), any()}) -> no_return().
init({Cb, IoDevice}) ->
  lager:info("Starting stdio server..."),
  ok = io:setopts(IoDevice, [binary]),
  ok = els_server:set_connection(IoDevice),
  ?MODULE:loop([], IoDevice, Cb, [return_maps]).

-spec send(any(), binary()) -> ok.
send(Connection, Payload) ->
  io:format(Connection, "~s", [Payload]).

%%==============================================================================
%% Listener loop function
%%==============================================================================

-spec loop([binary()], any(), function(), [any()]) -> no_return().
loop(Lines, IoDevice, Cb, JsonOpts) ->
  case io:get_line(IoDevice, "") of
    <<"\n">> ->
      Headers       = parse_headers(Lines),
      BinLength     = proplists:get_value(<<"content-length">>, Headers),
      Length        = binary_to_integer(BinLength),
      %% Use file:read/2 since it reads bytes
      {ok, Payload} = file:read(IoDevice, Length),
      Request       = jsx:decode(Payload, JsonOpts),
      Cb([Request]),
      ?MODULE:loop([], IoDevice, Cb, JsonOpts);
    eof ->
      Cb([#{
          <<"method">> => <<"exit">>,
          <<"params">> => []
        }]);
    Line ->
      ?MODULE:loop([Line | Lines], IoDevice, Cb, JsonOpts)
  end.

-spec parse_headers([binary()]) -> [{binary(), binary()}].
parse_headers(Lines) ->
  [parse_header(Line) || Line <- Lines].

-spec parse_header(binary()) -> {binary(), binary()}.
parse_header(Line) ->
  [Name, Value] = binary:split(Line, <<":">>),
  {string:trim(string:lowercase(Name)), string:trim(Value)}.