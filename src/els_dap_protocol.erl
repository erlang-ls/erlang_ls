%%==============================================================================
%% @doc Debug Adapter Protocol
%%
%% Handles the building and encoding of the messages supported by the
%% protocol.
%% @end
%%==============================================================================
-module(els_dap_protocol).

%%==============================================================================
%% Exports
%%==============================================================================
%% Messaging API
-export([ event/3
        , request/3
        , response/3
        , error_response/3
        ]).

%% Data Structures
-export([ range/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Messaging API
%%==============================================================================
-spec event(number(), binary(), any()) -> binary().
%% TODO: Body is optional
event(Seq, EventType, Body) ->
  Message = #{ seq => Seq
             , type => <<"event">>
             , event => EventType
             , body => Body
             },
  content(jsx:encode(Message)).

-spec request(number(), binary(), any()) -> binary().
request(RequestId, Method, Params) ->
  Message = #{ jsonrpc => ?JSONRPC_VSN
             , method  => Method
             , id      => RequestId
             , params  => Params
             },
  content(jsx:encode(Message)).

-spec response(number(), any(), any()) -> binary().
response(Seq, Command, Result) ->
  Message = #{ type  => <<"response">>
             , request_seq => Seq
             , success => true
             , command => Command
             , body  => Result
             },
  lager:debug("[Response] [message=~p]", [Message]),
  content(jsx:encode(Message)).

-spec error_response(number(), any(), any()) -> binary().
error_response(Seq, Command, Error) ->
  Message = #{ type  => <<"response">>
             , request_seq => Seq
             , success => false
             , command => Command
             , body  => #{ error => Error
                         }
             },
  lager:debug("[Response] [message=~p]", [Message]),
  content(jsx:encode(Message)).

%%==============================================================================
%% Data Structures
%%==============================================================================
-spec range(poi_range()) -> range().
range(#{ from := {FromL, FromC}, to := {ToL, ToC} }) ->
  #{ start => #{line => FromL - 1, character => FromC - 1}
   , 'end' => #{line => ToL - 1,   character => ToC - 1}
   }.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec content(binary()) -> binary().
content(Body) ->
els_utils:to_binary([headers(Body), "\r\n", Body]).

-spec headers(binary()) -> iolist().
headers(Body) ->
  io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]).
