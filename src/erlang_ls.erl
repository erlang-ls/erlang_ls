-module(erlang_ls).

-export([ main/1 ]).

-define(APP, erlang_ls).

-spec main([any()]) -> ok.
main(Args) ->
  %% Initialization
  ok = application:load(lager),
  ok = application:load(?APP),
  ok = parse_args(Args),
  ok = init_node_name(is_debug()),
  ok = lager_config(),
  %% Start the Erlang Language Server
  application:ensure_all_started(?APP),
  lager:info("Started erlang_ls server ~p", [node()]),
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
  application:set_env(?APP, log_dir, Dir),
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
  LogRoot  = log_root(),
  Handlers = case application:get_env(?APP, lager_handlers) of
               {ok, Value} -> Value;
               undefined   -> default_lager_handlers(LogRoot)
             end,
  ok = application:set_env(lager, handlers, Handlers),
  ok = application:set_env(lager, crash_log, "crash.log"),
  ok = application:set_env(lager, log_root, LogRoot),
  ok.

-spec default_lager_handlers(string()) -> [any()].
default_lager_handlers(LogRoot) ->
  LogFile = filename:join([LogRoot, "info.log"]),
  ok      = filelib:ensure_dir(LogFile),
  [ { lager_file_backend
    , [ {file, LogFile}
      , {level, info}
      ]
    }
  ].

-spec log_root() -> string().
log_root() ->
  {ok, LogDir}     = application:get_env(?APP, log_dir),
  {ok, CurrentDir} = file:get_cwd(),
  Dirname          = filename:basename(CurrentDir),
  filename:join([LogDir, Dirname]).

%%==============================================================================
%% Node name initialization
%%==============================================================================

-spec init_node_name(boolean()) -> ok.
init_node_name(true) ->
  ok       = els_utils:start_epmd(),
  Name     = "erlang_ls_" ++ integer_to_list(rand:uniform(16#FFFFFFFFF)),
  NodeName = list_to_atom(Name),
  net_kernel:start([NodeName, shortnames]),
  ok;
init_node_name(false) ->
  ok.

-spec is_debug() -> boolean().
is_debug() ->
  {ok, DebugMode} = application:get_env(?APP, debug_mode),
  DebugMode.
