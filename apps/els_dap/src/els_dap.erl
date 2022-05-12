%%=============================================================================
%% @doc Erlang DAP's escript Entrypoint
%%=============================================================================
-module(els_dap).

-export([main/1]).

-export([
    parse_args/1,
    log_root/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_dap.hrl").
-include_lib("kernel/include/logger.hrl").

-define(DEFAULT_LOGGING_LEVEL, "debug").

-spec main([any()]) -> ok.
main(Args) ->
    application:load(getopt),
    application:load(els_core),
    application:load(els_dap),
    ok = parse_args(Args),
    application:set_env(els_core, server, els_dap_server),
    configure_logging(),
    ?LOG_DEBUG("Ensure EPMD is running", []),
    0 = els_utils:cmd("epmd", ["-daemon"]),
    {ok, _} = application:ensure_all_started(?APP, permanent),
    patch_logging(),
    ?LOG_INFO("Started Erlang LS - DAP server", []),
    receive
        _ -> ok
    end.

-spec print_version() -> ok.
print_version() ->
    {ok, Vsn} = application:get_key(?APP, vsn),
    io:format("Version: ~s~n", [Vsn]),
    ok.

%%==============================================================================
%% Argument parsing
%%==============================================================================

-spec parse_args([string()]) -> ok.
parse_args(Args) ->
    case getopt:parse(opt_spec_list(), Args) of
        {ok, {[version | _], _BadArgs}} ->
            print_version(),
            halt(0);
        {ok, {ParsedArgs, _BadArgs}} ->
            set_args(ParsedArgs);
        {error, {invalid_option, _}} ->
            getopt:usage(opt_spec_list(), "Erlang LS - DAP"),
            halt(1)
    end.

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [
        {version, $v, "version", undefined, "Print the current version of Erlang LS - DAP"},
        {log_dir, $d, "log-dir", {string, filename:basedir(user_log, "els_dap")},
            "Directory where logs will be written."},
        {log_level, $l, "log-level", {string, ?DEFAULT_LOGGING_LEVEL},
            "The log level that should be used."}
    ].

-spec set_args([] | [getopt:compound_option()]) -> ok.
set_args([]) ->
    ok;
set_args([version | Rest]) ->
    set_args(Rest);
set_args([{Arg, Val} | Rest]) ->
    set(Arg, Val),
    set_args(Rest).

-spec set(atom(), getopt:arg_value()) -> ok.
set(log_dir, Dir) ->
    application:set_env(els_core, log_dir, Dir);
set(log_level, Level) ->
    application:set_env(els_core, log_level, list_to_atom(Level)).

%%==============================================================================
%% Logger configuration
%%==============================================================================

-spec configure_logging() -> ok.
configure_logging() ->
    LogFile = filename:join([log_root(), "dap_server.log"]),
    {ok, LoggingLevel} = application:get_env(els_core, log_level),
    ok = filelib:ensure_dir(LogFile),
    Handler = #{
        config => #{file => LogFile},
        level => LoggingLevel,
        formatter =>
            {logger_formatter, #{
                template => [
                    "[",
                    time,
                    "] ",
                    file,
                    ":",
                    line,
                    " ",
                    pid,
                    " ",
                    "[",
                    level,
                    "] ",
                    msg,
                    "\n"
                ]
            }}
    },
    [logger:remove_handler(H) || H <- logger:get_handler_ids()],
    logger:add_handler(els_core_handler, logger_std_h, Handler),
    logger:set_primary_config(level, LoggingLevel),
    ok.

-spec patch_logging() -> ok.
patch_logging() ->
    %% The ssl_handler is added by ranch -> ssl
    logger:remove_handler(ssl_handler),
    ok.

-spec log_root() -> string().
log_root() ->
    {ok, LogDir} = application:get_env(els_core, log_dir),
    {ok, CurrentDir} = file:get_cwd(),
    Dirname = filename:basename(CurrentDir),
    filename:join([LogDir, Dirname]).
