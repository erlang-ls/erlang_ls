-module(erlang_ls).

-export([ main/1 ]).

-export([ parse_args/1
        , log_root/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

-define(APP, els_lsp).
-define(DEFAULT_LOGGING_LEVEL, "info").

-spec main([any()]) -> ok.
main(Args) ->
  application:load(getopt),
  application:load(els_core),
  ok = parse_args(Args),
  application:set_env(els_core, server, els_server),
  configure_logging(),
  {ok, _} = application:ensure_all_started(?APP),
  patch_logging(),
  ?LOG_INFO("Started erlang_ls server", []),
  receive _ -> ok end.

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
      halt(1);
    {ok, {ParsedArgs, _BadArgs}} ->
      set_args(ParsedArgs);
    {error, {invalid_option, _}} ->
      getopt:usage(opt_spec_list(), "Erlang LS"),
      halt(1)
  end.

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
  [ { version
    , $v
    , "version"
    , undefined
    , "Print the current version of Erlang LS"
    }
  , { transport
    , $t
    , "transport"
    , {string, "stdio"}
    , "Specifies the transport the server will use for "
      "the connection with the client, either \"tcp\" or \"stdio\"."
    }
  , { port
    , $p
    , "port"
    , {integer, 10000}
    , "Used when the transport is tcp."
    }
 ,  { log_dir
    , $d
    , "log-dir"
    , {string, filename:basedir(user_log, "erlang_ls")}
    , "Directory where logs will be written."
    }
 ,  { log_level
    , $l
    , "log-level"
    , {string, ?DEFAULT_LOGGING_LEVEL}
    , "The log level that should be used."
    }
  ].

-spec set_args([] | [getopt:compound_option()]) -> ok.
set_args([]) -> ok;
set_args([version | Rest]) -> set_args(Rest);
set_args([{Arg, Val} | Rest]) ->
  set(Arg, Val),
  set_args(Rest).

-spec set(atom(), getopt:arg_value()) -> ok.
set(transport, Name) ->
  Transport = case Name of
                "tcp"   -> els_tcp;
                "stdio" -> els_stdio
              end,
  application:set_env(els_core, transport, Transport);
set(port, Port) ->
  application:set_env(els_core, port, Port);
set(log_dir, Dir) ->
  application:set_env(els_core, log_dir, Dir);
set(log_level, Level) ->
  application:set_env(els_core, log_level, list_to_atom(Level));
set(port_old, Port) ->
  application:set_env(els_core, port, Port).

%%==============================================================================
%% Logger configuration
%%==============================================================================

-spec configure_logging() -> ok.
configure_logging() ->
  LogFile = filename:join([log_root(), "server.log"]),
  {ok, LoggingLevel} = application:get_env(els_core, log_level),
  ok = filelib:ensure_dir(LogFile),
  Handler = #{ config => #{ file => LogFile }
             , level => LoggingLevel
             , formatter => { logger_formatter
                            , #{ template => [ "[", time, "] "
                                             , file, ":", line, " "
                                             , pid, " "
                                             , "[", level, "] "
                                             , msg, "\n"
                                             ]
                               }
                            }
             },
  [logger:remove_handler(H) || H <-  logger:get_handler_ids()],
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
