-module(erlang_ls_protocol).

-export([ request/4
        , response/3
        ]).

-include("erlang_ls.hrl").

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
  Content = content(jsx:encode(Message)),
  ok = tcp_send(Socket, Content).

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
