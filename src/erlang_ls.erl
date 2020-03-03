-module(erlang_ls).

-export([ main/1 ]).

-define(APP, erlang_ls).
-define(DEFAULT_LOGGING_LEVEL, "info").

-spec main([any()]) -> ok.
main(Args) ->
  %% Initialization
  application:load(lager),
  application:load(?APP),
  ok = parse_args(Args),
  ok = lager_config(),
  %% Start the Erlang Language Server
  application:ensure_all_started(?APP),
  lager:info("Started erlang_ls server", []),
  receive _ -> ok end.

%%==============================================================================
%% Argument parsing
%%==============================================================================

-spec parse_args([string()]) -> ok.
parse_args([]) ->
  ok;
parse_args(["--transport", Name | Rest]) ->
  Transport = case Name of
                "tcp"   -> els_tcp;
                "stdio" -> els_stdio
              end,
  application:set_env(?APP, transport, Transport),
  parse_args(Rest);
parse_args(["--port", Port | Rest]) ->
  application:set_env(?APP, port, list_to_integer(Port)),
  parse_args(Rest);
parse_args(["--log-dir", Dir | Rest]) ->
  application:set_env(?APP, logging_enabled, true),
  application:set_env(?APP, log_dir, Dir),
  parse_args(Rest);
parse_args(["--log-level", Level | Rest]) ->
  case Level of
    "none" -> ok;
    _      -> application:set_env(?APP, logging_enabled, true)
  end,
  application:set_env(?APP, log_level, Level),
  parse_args(Rest);
%% For backward compatibility with clients
parse_args([Port | Rest]) ->
  application:set_env(?APP, port, list_to_integer(Port)),
  parse_args(Rest).

%%==============================================================================
%% Lager configuration
%%==============================================================================


-spec lager_config() -> ok.
lager_config() ->
  LoggingEnabled = application:get_env(?APP, logging_enabled, false),
  case LoggingEnabled of
    true ->
      LogRoot   = log_root(),
      Handlers  = lager_handlers(LogRoot),
      ok = application:set_env(lager, handlers, Handlers),
      ok = application:set_env(lager, crash_log, "crash.log"),
      ok = application:set_env(lager, log_root, LogRoot);
    false ->
      ok = application:set_env(lager, handlers, []),
      ok = application:set_env(lager, crash_log, false)
  end.

-spec lager_handlers(string()) -> [any()].
lager_handlers(LogRoot) ->
  LogFile      = filename:join([LogRoot, "server.log"]),
  LoggingLevel = application:get_env(?APP, log_level, ?DEFAULT_LOGGING_LEVEL),
  ok           = filelib:ensure_dir(LogFile),
  [ { lager_file_backend
    , [ {file, LogFile}
      , {level, LoggingLevel}
      ]
    }
  ].

-spec log_root() -> string().
log_root() ->
  DefaultLogDir    = filename:basedir(user_log, "erlang_ls"),
  LogDir           = application:get_env(?APP, log_dir, DefaultLogDir),
  {ok, CurrentDir} = file:get_cwd(),
  Dirname          = filename:basename(CurrentDir),
  filename:join([LogDir, Dirname]).
