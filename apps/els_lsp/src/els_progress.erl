%%==============================================================================
%% Protocol support for $/progress
%%==============================================================================
-module(els_progress).

%%==============================================================================
%% Macros
%%==============================================================================
-define(METHOD, <<"$/progress">>).

%%==============================================================================
%% Exports
%%==============================================================================
-export([
    send_notification/2,
    token/0
]).

%%==============================================================================
%% Types
%%==============================================================================
-type token() :: binary().
-type value() :: els_work_done_progress:value().
-type params() :: #{
    token := token(),
    value := value()
}.
-export_type([
    token/0,
    params/0
]).

%%==============================================================================
%% API
%%==============================================================================
-spec send_notification(token(), value()) -> ok.
send_notification(Token, Value) ->
    Params = #{
        token => Token,
        value => Value
    },
    els_server:send_notification(?METHOD, Params).

-spec token() -> token().
token() ->
    list_to_binary(uuid:uuid_to_string(uuid:get_v4())).
