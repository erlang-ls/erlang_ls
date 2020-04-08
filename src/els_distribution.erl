-module(els_distribution).

-export([ ensure_epmd/0
        , ensure_node/1
        , start/1
        ]).

%% @doc Ensure the Erlang Port Mapper Daemon (EPMD) is up and running
-spec ensure_epmd() -> ok.
ensure_epmd() ->
  0 = els_utils:cmd("epmd", ["-daemon"]),
  ok.

%% @doc Start an Erlang node, if it is not already started
-spec ensure_node(atom()) -> ok.
ensure_node(Name) ->
  case net_kernel:connect_node(Name) of
    true ->
      lager:info("Connected to existing node [name=~p]", [Name]);
    false ->
      Cmd = els_config_runtime:get_start_cmd(),
      Args = els_config_runtime:get_start_args(),
      Path = els_config_runtime:get_otp_path(),
      lager:info("Using Erlang from ~s", [Path]),
      %% TODO: Do not just spawn
      spawn(fun() -> els_utils:cmd(Cmd, Args, Path) end)
  end.

%% @doc Turns a non-distributed node into a distributed one
-spec start(atom()) -> ok.
start(Name) ->
  ok = ensure_epmd(),
  lager:info("Starting new node [name=~p]", [Name]),
  net_kernel:start([Name, shortnames]),
  lager:info("New node started [name=~p]", [Name]).

%%==============================================================================
%% Internal functions
%%==============================================================================
