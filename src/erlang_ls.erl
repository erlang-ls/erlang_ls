-module(erlang_ls).

-export([ main/1 ]).

-export([ parse_args/1
        , lager_config/0
        , lager_handlers/1
        , log_root/0]).

-define(APP, erlang_ls).
-define(DEFAULT_LOGGING_LEVEL, "info").

-spec main([any()]) -> ok.
main(Args) ->
  %% Initialization
  application:load(lager),
  application:load(getopt),
  application:load(?APP),
  ok = parse_args(Args),
  ok = lager_config(),
  %% Start the Erlang Language Server
  application:ensure_all_started(?APP),
  lager:info("Started erlang_ls server", []),
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
  application:set_env(?APP, transport, Transport);
set(port, Port) ->
  application:set_env(?APP, port, Port);
set(log_dir, Dir) ->
  application:set_env(?APP, log_dir, Dir);
set(log_level, Level) ->
  application:set_env(?APP, log_level, Level);
set(port_old, Port) ->
  application:set_env(?APP, port, Port).

%%==============================================================================
%% Lager configuration
%%==============================================================================

-spec lager_config() -> ok.
lager_config() ->
  LoggingEnabled = application:get_env(?APP, logging_enabled, true),
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
  {ok, LoggingLevel} = application:get_env(?APP, log_level),
  ok           = filelib:ensure_dir(LogFile),
  [ { lager_file_backend
    , [ {file, LogFile}
      , {level, LoggingLevel}
      ]
    }
  ].

-spec log_root() -> string().
log_root() ->
  {ok, LogDir}     = application:get_env(?APP, log_dir),
  {ok, CurrentDir} = file:get_cwd(),
  Dirname          = filename:basename(CurrentDir),
  filename:join([LogDir, Dirname]).
