%%==============================================================================
%% The Language Server Protocol
%%==============================================================================
-module(erlang_ls_protocol).

%%==============================================================================
%% Exports
%%==============================================================================
%% Messaging API
-export([ notification/2
        , request/3
        , response/2
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
-spec notification(binary(), any()) ->
  {ok, any()}.
notification(Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec request(number(), binary(), any()) ->
  {ok, any()}.
request(RequestId, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , id      => RequestId
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec response(number(), any()) -> ok.
response(RequestId, Result) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , id      => RequestId
             , result  => Result
             },
  lager:debug("[Response] [message=~p]", [Message]),
  content(jsx:encode(Message)).

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
