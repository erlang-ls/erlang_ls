-module(els_dap_general_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , handle_info/2
        , is_enabled/0
        , init/0
        ]).

-export([ capabilities/0
        ]).

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
  #{threads => #{}}.

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
handle_request({<<"initialize">>, _Params}, State) ->
  {capabilities(), State};
handle_request({<<"launch">>, Params}, State) ->
  #{<<"cwd">> := Cwd} = Params,
  ok = file:set_cwd(Cwd),
  %% TODO: Do not hard-code sname
  spawn(fun() -> els_utils:cmd("rebar3", ["shell", "--sname", "daptoy"]) end),
  %% TODO: Wait until rebar3 node is started
  timer:sleep(3000),
  els_distribution_server:start_distribution(local_node()),
  net_kernel:connect_node(project_node()),
  %% TODO: Spawn could be un-necessary
  spawn(fun() -> els_dap_server:send_event(<<"initialized">>, #{}) end),
  {#{}, State};
handle_request({<<"configurationDone">>, _Params}, State) ->
  inject_dap_agent(project_node()),
  %% TODO: Fetch stack_trace mode from Launch Config
  rpc:call(project_node(), int, stack_trace, [all]),
  Args = [[break], {els_dap_agent, int_cb, [self()]}],
  rpc:call(project_node(), int, auto_attach, Args),
  %% TODO: Potentially fetch this from the Launch config
  rpc:cast(project_node(), daptoy_fact, fact, [5]),
  {#{}, State};
handle_request({<<"setBreakpoints">>, Params}, State) ->
  #{<<"source">> := #{<<"path">> := Path}} = Params,
  SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
  _SourceModified = maps:get(<<"sourceModified">>, Params, false),
  Module = els_uri:module(els_uri:uri(Path)),
  %% TODO: Keep a list of interpreted modules, not to re-interpret them
  rpc:call(project_node(), int, i, [Module]),
  [rpc:call(project_node(), int, break, [Module, Line]) ||
    #{<<"line">> := Line} <- SourceBreakpoints],
  Breakpoints = [#{<<"verified">> => true, <<"line">> => Line} ||
                  #{<<"line">> := Line} <- SourceBreakpoints],
  {#{<<"breakpoints">> => Breakpoints}, State};
handle_request({<<"setExceptionBreakpoints">>, _Params}, State) ->
  {#{}, State};
handle_request({<<"threads">>, _Params}, #{threads := Threads0} = State) ->
  Threads = [#{ <<"id">> => Id
              , <<"name">> => unicode:characters_to_binary(lists:flatten(io_lib:format("~p", [Pid])))
              } || {Id, Pid} <- maps:to_list(Threads0)],
  {#{<<"threads">> => Threads}, State};
handle_request({<<"stackTrace">>, Params}, #{threads := Threads} = State) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = maps:get(ThreadId, Threads),
  %% TODO: Abstract RPC into a function
  {ok, Meta} =  rpc:call(project_node(), dbg_iserver, safe_call, [{get_meta, Pid}]),
  %% TODO: Also examine rest of list
  [{_Level, {M, F, A}}|_] = rpc:call(project_node(), int, meta, [Meta, backtrace, all]),
  StackFrame = #{ <<"id">> => erlang:unique_integer([positive])
                , <<"name">> => unicode:characters_to_binary(io_lib:format("~p:~p/~p", [M, F, length(A)]))
                , <<"line">> => 0
                , <<"column">> => 0
                },
  {#{<<"stackFrames">> => [StackFrame]}, State};
handle_request({<<"scopes">>, Params}, State) ->
  #{<<"frameId">> := _FrameId} = Params,
  {#{<<"scopes">> => []}, State};
handle_request({<<"next">>, Params}, #{threads := Threads} = State) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = maps:get(ThreadId, Threads),
  ok = rpc:call(project_node(), int, next, [Pid]),
  {#{}, State};
handle_request({<<"continue">>, Params}, #{threads := Threads} = State) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = maps:get(ThreadId, Threads),
  ok = rpc:call(project_node(), int, continue, [Pid]),
  {#{}, State}.

-spec handle_info(any(), state()) -> state().
handle_info({int_cb, Thread}, #{threads := Threads} = State) ->
  lager:debug("Int CB called. thread=~p", [Thread]),
  ThreadId = id(Thread),
  els_dap_server:send_event(<<"stopped">>, #{ <<"reason">> => <<"breakpoint">>
                                            , <<"threadId">> => ThreadId
                                            }),
  State#{threads => maps:put(ThreadId, Thread, Threads)}.

%%==============================================================================
%% API
%%==============================================================================

-spec capabilities() -> capabilities().
capabilities() ->
  #{}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec inject_dap_agent(atom()) -> ok.
inject_dap_agent(Node) ->
  Module = els_dap_agent,
  {Module, Bin, File} = code:get_object_code(Module),
  {_Replies, _} = rpc:call(Node, code, load_binary, [Module, File, Bin]),
  ok.

-spec project_node() -> atom().
project_node() ->
  %% TODO: Do not hard-code node name
  {ok, Hostname} = inet:gethostname(),
  list_to_atom("daptoy@" ++ Hostname).

-spec local_node() -> atom().
local_node() ->
  %% TODO: Do not hard-code node name
  {ok, Hostname} = inet:gethostname(),
  list_to_atom("dap@" ++ Hostname).

-spec id(pid()) -> integer().
id(Pid) ->
  erlang:phash2(Pid).
