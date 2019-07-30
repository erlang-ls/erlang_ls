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
-export([ range/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Messaging API
%%==============================================================================
-spec notification(binary(), any()) -> any().
notification(Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec request(number(), binary(), any()) -> any().
request(RequestId, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , id      => RequestId
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec response(number(), any()) -> any().
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
-spec range(erlang_ls_parser:range()) -> range().
range(#{ from := {FromL, FromC}, to := {ToL, ToC} }) ->
  #{ start => #{line => FromL, character => FromC}
   , 'end' => #{line => ToL,   character => ToC}
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
