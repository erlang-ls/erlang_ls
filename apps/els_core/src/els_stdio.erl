-module(els_stdio).

-export([
    start_listener/1,
    init/1,
    send/2
]).

-export([loop/4]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% els_transport callbacks
%%==============================================================================
-spec start_listener(function()) -> {ok, pid()}.
start_listener(Cb) ->
    IoDevice = application:get_env(els_core, io_device, standard_io),
    {ok, proc_lib:spawn_link(?MODULE, init, [{Cb, IoDevice}])}.

-spec init({function(), atom() | pid()}) -> no_return().
init({Cb, IoDevice}) ->
    ?LOG_INFO("Starting stdio server... [io_device=~p]", [IoDevice]),
    ok = io:setopts(IoDevice, [binary, {encoding, latin1}]),
    {ok, Server} = application:get_env(els_core, server),
    ok = Server:set_io_device(IoDevice),
    ?MODULE:loop([], IoDevice, Cb, [return_maps]).

-spec send(atom() | pid(), binary()) -> ok.
send(IoDevice, Payload) ->
    io:format(IoDevice, "~s", [Payload]).

%%==============================================================================
%% Listener loop function
%%==============================================================================

-spec loop([binary()], any(), function(), [any()]) -> no_return().
loop(Lines, IoDevice, Cb, JsonOpts) ->
    case io:get_line(IoDevice, "") of
        <<"\n">> ->
            Headers = parse_headers(Lines),
            BinLength = proplists:get_value(<<"content-length">>, Headers),
            Length = binary_to_integer(BinLength),
            %% Use file:read/2 since it reads bytes
            {ok, Payload} = file:read(IoDevice, Length),
            Request = jsx:decode(Payload, JsonOpts),
            Cb([Request]),
            ?MODULE:loop([], IoDevice, Cb, JsonOpts);
        eof ->
            Cb([
                #{
                    <<"method">> => <<"exit">>,
                    <<"params">> => []
                }
            ]);
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
