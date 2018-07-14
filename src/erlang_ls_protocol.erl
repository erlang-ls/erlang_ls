%%==============================================================================
%% The Language Server Protocol
%%==============================================================================
-module(erlang_ls_protocol).

%%==============================================================================
%% Exports
%%==============================================================================
%% Messaging API
-export([ notification/3
        , request/4
        , response/3
        ]).

%% Data Structures
-export([ range/1
        , range/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Messaging API
%%==============================================================================
-spec notification(gen_tcp:socket(), binary(), any()) ->
  {ok, any()}.
notification(Socket, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , params  => Params
             },
  Content = content(jsx:encode(Message)),
  ok = tcp_send(Socket, Content).

-spec request(gen_tcp:socket(), number(), binary(), any()) ->
  {ok, any()}.
request(Socket, RequestId, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , id      => RequestId
             , params  => Params
             },
  Content = content(jsx:encode(Message)),
  ok = tcp_send(Socket, Content),
  tcp_receive(Socket).

-spec response(gen_tcp:socket(), number(), any()) -> ok.
response(Socket, RequestId, Result) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , id      => RequestId
             , result  => Result
             },
  lager:debug("[Response] [message=~p]", [Message]),
  Content = content(jsx:encode(Message)),
  ok = tcp_send(Socket, Content).

%%==============================================================================
%% Data Structures
%%==============================================================================
-spec range(integer()) -> range().
range(FromLine) ->
  range(FromLine, FromLine).

-spec range(integer(), integer()) -> range().
range(FromLine, ToLine) ->
  #{ start => #{line => FromLine, character => 0}
   , 'end' => #{line => ToLine,   character => 0}
   }.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec content(binary()) -> iolist().
content(Body) ->
  [headers(Body), "\r\n", Body].

-spec headers(binary()) -> iolist().
headers(Body) ->
  io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]).

-spec tcp_send(gen_tcp:socket(), iodata()) -> ok.
tcp_send(Socket, Content) ->
  ok = gen_tcp:send(Socket, Content).

-spec tcp_receive(gen_tcp:socket()) -> {ok, binary()}.
tcp_receive(Socket) ->
  {ok, Packet}    = gen_tcp:recv(Socket, 0),
  {Headers, Body} = cow_http:parse_headers(Packet),
  BinLength       = proplists:get_value( <<"content-length">>, Headers),
  Length          = binary_to_integer(BinLength),
  tcp_receive(Socket, Body, Length).

-spec tcp_receive(gen_tcp:socket(), binary(), non_neg_integer()) ->
  {ok, binary()}.
tcp_receive(Socket, Body, Length) ->
  case byte_size(Body) < Length of
    true ->
      {ok, Packet} = gen_tcp:recv(Socket, 0),
      tcp_receive(Socket, <<Body/binary, Packet/binary>>, Length);
    false ->
      {ok, jsx:decode(Body, [return_maps, {labels, atom}])}
  end.
