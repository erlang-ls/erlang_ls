-module(els_log_notification).

-include_lib("els_lsp/include/els_lsp.hrl").

-export([log/2]).

-define(LSP_MESSAGE_TYPE_ERROR, 1).
-define(LSP_MESSAGE_TYPE_WARNING, 2).
-define(LSP_MESSAGE_TYPE_INFO, 3).
-define(LSP_MESSAGE_TYPE_LOG, 4).

-type lsp_message_type() :: ?LSP_MESSAGE_TYPE_ERROR |
                            ?LSP_MESSAGE_TYPE_WARNING |
                            ?LSP_MESSAGE_TYPE_INFO |
                            ?LSP_MESSAGE_TYPE_LOG.

-spec log(logger:log_event(), logger:config_handler()) -> ok.
log(#{level := Level} = LogEvent, _Config) ->
    try
        Msg = logger_formatter:format( LogEvent
                                     , #{ template => ?LSP_LOG_FORMAT}),
        els_server:send_notification(<<"window/logMessage">>, #{
            <<"message">> => unicode:characters_to_binary(Msg),
            <<"type">> => otp_log_level_to_lsp(Level)
        })
    catch
        E:R:ST ->
            ErrMsg =
                io_lib:format( "Logger Exception ({~w, ~w}): ~n~p"
                             , [E, R, ST]),
            els_server:send_notification(<<"window/logMessage">>, #{
                <<"message">> => unicode:characters_to_binary(ErrMsg),
                <<"type">> => 1
            })
    end.

-spec otp_log_level_to_lsp(logger:level()) -> lsp_message_type().
otp_log_level_to_lsp(emergency) -> ?LSP_MESSAGE_TYPE_ERROR;
otp_log_level_to_lsp(alert) -> ?LSP_MESSAGE_TYPE_ERROR;
otp_log_level_to_lsp(critical) -> ?LSP_MESSAGE_TYPE_ERROR;
otp_log_level_to_lsp(error) -> ?LSP_MESSAGE_TYPE_ERROR;
otp_log_level_to_lsp(warning) -> ?LSP_MESSAGE_TYPE_WARNING;
otp_log_level_to_lsp(notice) -> ?LSP_MESSAGE_TYPE_INFO;
otp_log_level_to_lsp(info) -> ?LSP_MESSAGE_TYPE_LOG;
otp_log_level_to_lsp(debug) -> ?LSP_MESSAGE_TYPE_LOG.
