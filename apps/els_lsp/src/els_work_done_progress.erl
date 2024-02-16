%%==============================================================================
%% Protocol support for workDoneProgress
%%==============================================================================
-module(els_work_done_progress).

%%==============================================================================
%% Includes
%%==============================================================================
%%==============================================================================
%% Types
%%==============================================================================
-type percentage() :: 0..100.
-type value_begin() :: #{
    kind := 'begin',
    title := binary(),
    cancellable => boolean(),
    message => binary(),
    percentage => percentage()
}.
-type value_report() :: #{
    kind := 'report',
    cancellable => boolean(),
    message => binary(),
    percentage => percentage()
}.
-type value_end() :: #{
    kind := 'end',
    message => binary()
}.
-type value() ::
    value_begin()
    | value_report()
    | value_end().

-export_type([
    value_begin/0,
    value_report/0,
    value_end/0,
    value/0
]).

%%==============================================================================
%% Exports
%%==============================================================================

-export([
    is_supported/0,
    send_create_request/0,
    value_begin/2,
    value_begin/3,
    value_report/1,
    value_report/2,
    value_end/1
]).

%%==============================================================================
%% API
%%==============================================================================

-spec is_supported() -> boolean().
is_supported() ->
    case els_config:get(capabilities) of
        #{<<"window">> := #{<<"workDoneProgress">> := WorkDoneProgress}} when
            is_boolean(WorkDoneProgress)
        ->
            WorkDoneProgress;
        _ ->
            false
    end.

-spec send_create_request() -> els_progress:token().
send_create_request() ->
    Token = els_progress:token(),
    Method = <<"window/workDoneProgress/create">>,
    Params = #{token => Token},
    ok = els_server:send_request(Method, Params),
    Token.

-spec value_begin(binary(), binary()) -> value_begin().
value_begin(Title, Message) ->
    #{
        kind => 'begin',
        title => Title,
        cancellable => false,
        message => Message
    }.

-spec value_begin(binary(), binary(), percentage()) -> value_begin().
value_begin(Title, Message, Percentage) ->
    #{
        kind => 'begin',
        title => Title,
        cancellable => false,
        message => Message,
        percentage => Percentage
    }.

-spec value_report(binary()) -> value_report().
value_report(Message) ->
    #{
        kind => 'report',
        cancellable => false,
        message => Message
    }.

-spec value_report(binary(), percentage()) -> value_report().
value_report(Message, Percentage) ->
    #{
        kind => 'report',
        cancellable => false,
        message => Message,
        percentage => Percentage
    }.

-spec value_end(binary()) -> value_end().
value_end(Message) ->
    #{
        kind => 'end',
        message => Message
    }.
