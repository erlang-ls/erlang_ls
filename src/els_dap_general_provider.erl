-module(els_dap_general_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , handle_info/2
        , is_enabled/0
        , init/0
        ]).

-export([ capabilities/0
        ]).

-export([ send_int_cb/2 ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================

-type capabilities() :: #{}.
-type initialize_request() :: {initialize, initialize_params()}.
-type initialize_params() :: #{ processId             := number() | null
                              , rootPath              => binary() | null
                              , rootUri               := uri() | null
                              , initializationOptions => any()
                              , capabilities          := client_capabilities()
                              , trace                 => off
                                                       | messages
                                                       | verbose
                              , workspaceFolders      => [workspace_folder()]
                                                       | null
                              }.
-type initialize_result() :: capabilities().
-type initialized_request() :: {initialized, initialized_params()}.
-type initialized_params() :: #{}.
-type initialized_result() :: null.
-type shutdown_request() :: {shutdown, shutdown_params()}.
-type shutdown_params() :: #{}.
-type shutdown_result() :: null.
-type exit_request() :: {exit, exit_params()}.
-type exit_params() :: #{status => atom()}.
-type exit_result() :: null.
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec init() -> #{}.
init() ->
  #{threads => []}.

-spec handle_request( initialize_request()
                    | initialized_request()
                    | shutdown_request()
                    | exit_request()
                    , state()) ->
        { initialize_result()
        | initialized_result()
        | shutdown_result()
        | exit_result()
        , state()
        }.
handle_request({initialize, _Params}, State) ->
  {capabilities(), State};
handle_request({launch, Params}, State) ->
  #{<<"cwd">> := Cwd} = Params,
  %% TODO: Launch a rocket
  ok = file:set_cwd(Cwd),
  %% TODO: Do not hard-code sname
  spawn(fun() -> els_utils:cmd("rebar3", ["shell", "--sname", "daptoy"]) end),
  {ok, Hostname} = inet:gethostname(),
  ProjectNode = list_to_atom("daptoy@" ++ Hostname),
  LocalNode = list_to_atom("dap@" ++ Hostname),
  %% TODO: Wait until rebar3 node is started
  timer:sleep(3000),
  els_distribution_server:start_distribution(LocalNode),
  net_kernel:connect_node(ProjectNode),
  %% TODO: Spawn could be un-necessary
  spawn(fun() -> els_dap_server:send_event(<<"initialized">>, #{}) end),
  {#{}, State};
handle_request({configuration_done, _Params}, State) ->
  int:auto_attach([break], {?MODULE, send_int_cb, [self()]}),
  %% spawn(fun() -> timer:sleep(500), daptoy_fact:fact(5) end),
  {#{}, State};
handle_request({set_breakpoints, Params}, State) ->
  #{<<"source">> := #{<<"path">> := Path}} = Params,
  SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
  _SourceModified = maps:get(<<"sourceModified">>, Params, false),
  Module = els_uri:module(els_uri:uri(Path)),
  %% TODO: Keep a list of interpreted modules, not to re-interpret them
  int:ni(Module),
  [int:break(Module, Line) || #{<<"line">> := Line} <- SourceBreakpoints],
  Breakpoints = [#{<<"verified">> => true, <<"line">> => Line} ||
                  #{<<"line">> := Line} <- SourceBreakpoints],
  {#{<<"breakpoints">> => Breakpoints}, State};
handle_request({set_exception_breakpoints, _Params}, State) ->
  {#{}, State};
handle_request({threads, _Params}, #{threads := Threads0} = State) ->
  Threads = [#{ <<"id">> => erlang:phash2(Pid)
              , <<"name">> => unicode:characters_to_binary(lists:flatten(io_lib:format("~p", [Pid])))
              } || Pid <- Threads0],
  {#{<<"threads">> => Threads}, State};
handle_request({initialized, _Params}, State) ->
  #{root_uri := RootUri, init_options := InitOptions} = State,
  DbDir = application:get_env(erlang_ls, db_dir, default_db_dir()),
  OtpPath = els_config:get(otp_path),
  NodeName = node_name(RootUri, els_utils:to_binary(OtpPath)),
  els_db:install(NodeName, DbDir),
  case maps:get(<<"indexingEnabled">>, InitOptions, true) of
    true  -> els_indexing:start();
    false -> lager:info("Skipping Indexing (disabled via InitOptions)")
  end,
  {null, State};
handle_request({shutdown, _Params}, State) ->
  {null, State};
handle_request({exit, #{status := Status}}, State) ->
  lager:info("Language server stopping..."),
  ExitCode = case Status of
               shutdown -> 0;
               _        -> 1
             end,
  els_utils:halt(ExitCode),
  {null, State}.

-spec handle_info(any(), state()) -> state().
handle_info({int_cb, Thread}, #{threads := Threads} = State) ->
  lager:debug("Int CB called. thread=~p", [Thread]),
  State#{threads => [Thread|Threads]}.

%%==============================================================================
%% API
%%==============================================================================

-spec capabilities() -> capabilities().
capabilities() ->
  #{}.

%%==============================================================================
%% Callbacks
%%==============================================================================

-spec send_int_cb(pid(), pid()) -> ok.
send_int_cb(Thread, ProviderPid) ->
  ProviderPid ! {int_cb, Thread}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec node_name(uri(), binary()) -> atom().
node_name(RootUri, OtpPath) ->
  <<SHA:160/integer>> = crypto:hash(sha, <<RootUri/binary, OtpPath/binary>>),
  list_to_atom(lists:flatten(io_lib:format("erlang_ls_~40.16.0b", [SHA]))).

-spec default_db_dir() -> string().
default_db_dir() ->
  filename:basedir(user_cache, "erlang_ls").
