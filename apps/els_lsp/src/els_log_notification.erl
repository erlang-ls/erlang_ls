-module(els_log_notification).

-include_lib("els_lsp/include/els_lsp.hrl").

-export([log/2]).

-type lsp_message_type() :: 1 | 2 | 3 | 4.

-define(LSP_MESSAGE_TYPE_ERROR, 1).
-define(LSP_MESSAGE_TYPE_WARNING, 2).
-define(LSP_MESSAGE_TYPE_INFO, 3).
-define(LSP_MESSAGE_TYPE_LOG, 4).


-spec log(logger:log_event(), logger:config_handler()) -> ok.
log(#{level := Level} = LogEvent, _Config) ->
    try
        Msg = logger_formatter:format( LogEvent
                                     , #{ template => ?ELP_LOG_FORMAT}),
        els_server:send_notification(<<"window/logMessage">>, #{
            <<"message">> => unicode:characters_to_binary(Msg),
            <<"type">> => map_otp_log_level_to_lsp(Level)
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

-spec map_otp_log_level_to_lsp(logger:level()) -> lsp_message_type().
map_otp_log_level_to_lsp(emergency) -> ?LSP_MESSAGE_TYPE_ERROR;
map_otp_log_level_to_lsp(alert) -> ?LSP_MESSAGE_TYPE_ERROR;
map_otp_log_level_to_lsp(critical) -> ?LSP_MESSAGE_TYPE_ERROR;
map_otp_log_level_to_lsp(error) -> ?LSP_MESSAGE_TYPE_ERROR;
map_otp_log_level_to_lsp(warning) -> ?LSP_MESSAGE_TYPE_WARNING;
map_otp_log_level_to_lsp(notice) -> ?LSP_MESSAGE_TYPE_INFO;
map_otp_log_level_to_lsp(info) -> ?LSP_MESSAGE_TYPE_LOG;
map_otp_log_level_to_lsp(debug) -> ?LSP_MESSAGE_TYPE_LOG.
