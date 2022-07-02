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
-export([
    event/3,
    request/3,
    response/3,
    error_response/3
]).

%% Data Structures
-export([range/1]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_dap.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Messaging API
%%==============================================================================

-spec event(number(), binary(), map()) -> binary().
event(Seq, <<"initialized">> = EventType, _Body) ->
    %% The initialized event has no body.
    Message = #{
        type => <<"event">>,
        seq => Seq,
        event => EventType
    },
    content(jsx:encode(Message));
event(Seq, EventType, Body) ->
    Message = #{
        type => <<"event">>,
        seq => Seq,
        event => EventType,
        body => Body
    },
    content(jsx:encode(Message)).

-spec request(number(), binary(), any()) -> binary().
request(RequestSeq, Method, Params) ->
    Message = #{
        type => <<"request">>,
        seq => RequestSeq,
        command => Method,
        arguments => Params
    },
    content(jsx:encode(Message)).

-spec response(number(), any(), any()) -> binary().
response(Seq, Command, Result) ->
    Message = #{
        type => <<"response">>,
        request_seq => Seq,
        success => true,
        command => Command,
        body => Result
    },
    ?LOG_DEBUG("[Response] [message=~p]", [Message]),
    content(jsx:encode(Message)).

-spec error_response(number(), any(), binary()) -> binary().
error_response(Seq, Command, Error) ->
    Message = #{
        type => <<"response">>,
        request_seq => Seq,
        success => false,
        command => Command,
        body => #{
            error => #{
                id => Seq,
                format => Error,
                showUser => true
            }
        }
    },
    ?LOG_DEBUG("[Response] [message=~p]", [Message]),
    content(jsx:encode(Message)).

%%==============================================================================
%% Data Structures
%%==============================================================================
-spec range(els_poi:poi_range()) -> range().
range(#{from := {FromL, FromC}, to := {ToL, ToC}}) ->
    #{
        start => #{line => FromL - 1, character => FromC - 1},
        'end' => #{line => ToL - 1, character => ToC - 1}
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
