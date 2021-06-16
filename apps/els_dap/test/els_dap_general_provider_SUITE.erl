-module(els_dap_general_provider_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

%% Test cases
-export([ initialize/1
        , launch_mfa/1
        , launch_mfa_with_cookie/1
        , configuration_done/1
        , configuration_done_with_long_names/1
        , configuration_done_with_long_names_using_host/1
        , configuration_done_with_breakpoint/1
        , frame_variables/1
        , navigation_and_frames/1
        , set_variable/1
        , breakpoints/1
        , project_node_exit/1
        , breakpoints_with_cond/1
        , breakpoints_with_hit/1
        , breakpoints_with_cond_and_hit/1
        , log_points/1
        , log_points_with_lt_condition/1
        , log_points_with_eq_condition/1
        , log_points_with_hit/1
        , log_points_with_hit1/1
        , log_points_with_cond_and_hit/1
        , log_points_empty_cond/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
  [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
  els_dap_test_utils:all(?MODULE).

-spec groups() -> [atom()].
groups() ->
  [].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
   meck:unload().

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) when
  TestCase =:= undefined orelse
    TestCase =:= initialize orelse
    TestCase =:= launch_mfa orelse
    TestCase =:= launch_mfa_with_cookie orelse
    TestCase =:= configuration_done orelse
    TestCase =:= configuration_done_with_long_names orelse
    TestCase =:= configuration_done_with_long_names_using_host orelse
    TestCase =:= configuration_done_with_breakpoint orelse
    TestCase =:= log_points orelse
    TestCase =:= log_points_with_lt_condition orelse
    TestCase =:= log_points_with_eq_condition orelse
    TestCase =:= log_points_with_hit orelse
    TestCase =:= log_points_with_hit1 orelse
    TestCase =:= log_points_with_cond_and_hit orelse
    TestCase =:= log_points_empty_cond orelse
    TestCase =:= breakpoints_with_cond orelse
    TestCase =:= breakpoints_with_hit orelse
    TestCase =:= breakpoints_with_cond_and_hit ->
  {ok, DAPProvider} = els_provider:start_link(els_dap_general_provider),
  {ok, _} = els_config:start_link(),
  meck:expect(els_dap_server, send_event, 2, meck:val(ok)),
  [{provider, DAPProvider}, {node, node_name()} | Config];
init_per_testcase(_TestCase, Config0) ->
  Config1 = init_per_testcase(undefined, Config0),
  %% initialize dap, equivalent to configuration_done_with_breakpoint
  try configuration_done_with_breakpoint(Config1) of
    ok -> Config1;
    R -> {user_skip, {error, dap_initialization, R}}
  catch
    Class:Reason ->
      {user_skip, {error, dap_initialization, Class, Reason}}
  end.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  NodeName = ?config(node, Config),
  Node = binary_to_atom(NodeName, utf8),
  unset_all_env(els_core),
  ok = gen_server:stop(?config(provider, Config)),
  gen_server:stop(els_config),
  %% kill the project node
  rpc:cast(Node, erlang, halt, []),
  ok.

%%==============================================================================
%% Helpers
%%==============================================================================
-spec unset_all_env(atom()) -> ok.
unset_all_env(Application) ->
  Envs = application:get_all_env(Application),
  unset_env(Application, Envs).

-spec unset_env(atom(), list({atom(), term()})) -> ok.
unset_env(_Application, []) ->
  ok;
unset_env(Application, [{Par, _Val} | Rest]) ->
  application:unset_env(Application, Par),
  unset_env(Application, Rest).

-spec node_name() -> node().
node_name() ->
  unicode:characters_to_binary(
    io_lib:format("~s~p@localhost", [?MODULE, erlang:unique_integer()])
  ).

-spec path_to_test_module(file:name(), module()) -> file:name().
path_to_test_module(AppDir, Module) ->
  unicode:characters_to_binary(
    io_lib:format("~s.erl", [filename:join([AppDir, "src", Module])])
  ).

-spec wait_for_break(binary(), module(), non_neg_integer()) -> boolean().
wait_for_break(NodeName, WantModule, WantLine) ->
  Node = binary_to_atom(NodeName, utf8),
  Checker =
    fun() ->
      Snapshots = rpc:call(Node, int, snapshot, []),
      lists:any(
        fun
          ({_, _, break, {Module, Line}}) when
            Module =:= WantModule andalso Line =:= WantLine
          ->
            true;
          (_) ->
            false
        end,
        Snapshots
      )
    end,
  els_dap_test_utils:wait_for_fun(Checker, 200, 20).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec initialize(config()) -> ok.
initialize(Config) ->
  Provider = ?config(provider, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  ok.

-spec launch_mfa(config()) -> ok.
launch_mfa(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, els_dap_test_module, entry, [])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  ok.

-spec launch_mfa_with_cookie(config()) -> ok.
launch_mfa_with_cookie(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, <<"some_cookie">>,
                   els_dap_test_module, entry, [])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  ok.

-spec configuration_done(config()) -> ok.
configuration_done(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, els_dap_test_module, entry, [])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  els_provider:handle_request(Provider, request_configuration_done(#{})),
  ok.

-spec configuration_done_with_long_names(config()) -> ok.
configuration_done_with_long_names(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  NodeStr = io_lib:format("~s~p", [?MODULE, erlang:unique_integer()]),
  Node = unicode:characters_to_binary(NodeStr),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, <<"some_cookie">>,
                   els_dap_test_module, entry, [], use_long_names)
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  ok.

-spec configuration_done_with_long_names_using_host(config()) -> ok.
configuration_done_with_long_names_using_host(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, <<"some_cookie">>,
                   els_dap_test_module, entry, [], use_long_names)
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  ok.

-spec configuration_done_with_breakpoint(config()) -> ok.
configuration_done_with_breakpoint(Config) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, els_dap_test_module, entry, [5])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),

  els_provider:handle_request(
    Provider,
    request_set_breakpoints( path_to_test_module(DataDir, els_dap_test_module)
                           , [9, 29])
  ),
  els_provider:handle_request(Provider, request_configuration_done(#{})),
  ?assertEqual(ok, wait_for_break(Node, els_dap_test_module, 9)),
  ok.

-spec frame_variables(config()) -> ok.
frame_variables(Config) ->
  Provider = ?config(provider, Config),
  %% get thread ID from mocked DAP response
  #{ <<"reason">> := <<"breakpoint">>
   , <<"threadId">> := ThreadId} =
    meck:capture(last, els_dap_server, send_event, [<<"stopped">>, '_'], 2),
  %% get stackframe
  #{<<"stackFrames">> := [#{<<"id">> := FrameId}]} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  %% get scope
  #{<<"scopes">> := [#{<<"variablesReference">> := VariableRef}]} =
    els_provider:handle_request(Provider, request_scope(FrameId)),
  %% extract variable
  #{<<"variables">> := [NVar]} =
    els_provider:handle_request(Provider, request_variable(VariableRef)),
  %% at this point there should be only one variable present,
  ?assertMatch(#{ <<"name">> := <<"N">>
                , <<"value">> := <<"5">>
                , <<"variablesReference">> := 0
                }
              , NVar),
  ok.

-spec navigation_and_frames(config()) -> ok.
navigation_and_frames(Config) ->
  %% test next, stepIn, continue and check aginst expeted stack frames
  Provider = ?config(provider, Config),
  #{<<"threads">> := [#{<<"id">> := ThreadId}]} =
    els_provider:handle_request( Provider
                               , request_threads()
                               ),
  %% next
  %%, reset meck history, to capture next call
  meck:reset([els_dap_server]),
  els_provider:handle_request(Provider, request_next(ThreadId)),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  %% check
  #{<<"stackFrames">> := Frames1} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  ?assertMatch([#{ <<"line">> := 11
                 , <<"name">> := <<"els_dap_test_module:entry/1">>}], Frames1),
  %% continue
  meck:reset([els_dap_server]),
  els_provider:handle_request(Provider, request_continue(ThreadId)),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  %% check
  #{<<"stackFrames">> := Frames2} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  ?assertMatch( [ #{ <<"line">> := 9
                   , <<"name">> := <<"els_dap_test_module:entry/1">>}
                , #{ <<"line">> := 11
                   , <<"name">> := <<"els_dap_test_module:entry/1">>}
                ]
              , Frames2
              ),
  %% stepIn
  meck:reset([els_dap_server]),
  els_provider:handle_request(Provider, request_step_in(ThreadId)),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  %% check
  #{<<"stackFrames">> := Frames3} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  ?assertMatch( [ #{ <<"line">> := 15
                   , <<"name">> := <<"els_dap_test_module:ds/0">>
                  },
                  #{ <<"line">> := 9
                   , <<"name">> := <<"els_dap_test_module:entry/1">>},
                  #{ <<"line">> := 11
                   , <<"name">> := <<"els_dap_test_module:entry/1">>}
                ]
              , Frames3
              ),
  ok.

-spec set_variable(config()) -> ok.
set_variable(Config) ->
  Provider = ?config(provider, Config),
  #{<<"threads">> := [#{<<"id">> := ThreadId}]} =
    els_provider:handle_request( Provider
                               , request_threads()
                               ),
  #{<<"stackFrames">> := [#{<<"id">> := FrameId1}]} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  meck:reset([els_dap_server]),
  Result1 =
    els_provider:handle_request( Provider
                               , request_evaluate( <<"repl">>
                                                 , FrameId1
                                                 , <<"N=1">>
                                                 )
                               ),
  ?assertEqual(#{<<"result">> => <<"1">>}, Result1),

  %% get variable value through hover evaluate
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  #{<<"stackFrames">> := [#{<<"id">> := FrameId2}]} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  ?assertNotEqual(FrameId1, FrameId2),
  Result2 =
    els_provider:handle_request( Provider
                               , request_evaluate( <<"hover">>
                                                 , FrameId2
                                                 , <<"N">>
                                                 )
                               ),
  ?assertEqual(#{<<"result">> => <<"1">>}, Result2),
  %% get variable value through scopes
  #{ <<"scopes">> := [ #{<<"variablesReference">> := VariableRef} ] } =
    els_provider:handle_request(Provider, request_scope(FrameId2)),
  %% extract variable
  #{<<"variables">> := [NVar]} =
    els_provider:handle_request( Provider
                               , request_variable(VariableRef)
                               ),
  %% at this point there should be only one variable present
  ?assertMatch( #{ <<"name">> := <<"N">>
                 , <<"value">> := <<"1">>
                 , <<"variablesReference">> := 0
                 }
              , NVar
              ),
    ok.

-spec breakpoints(config()) -> ok.
breakpoints(Config) ->
  Provider = ?config(provider, Config),
  NodeName = ?config(node, Config),
  Node = binary_to_atom(NodeName, utf8),
  DataDir = ?config(data_dir, Config),
  els_provider:handle_request(
    Provider,
    request_set_breakpoints( path_to_test_module(DataDir, els_dap_test_module)
                           , [9])
  ),
  ?assertMatch([{{els_dap_test_module, 9}, _}], els_dap_rpc:all_breaks(Node)),
  els_provider:handle_request(
    Provider,
    request_set_function_breakpoints([<<"els_dap_test_module:entry/1">>])
  ),
  ?assertMatch(
    [{{els_dap_test_module, 7}, _}, {{els_dap_test_module, 9}, _}],
    els_dap_rpc:all_breaks(Node)
  ),
  els_provider:handle_request(
    Provider,
    request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module)
                           , [])
  ),
  ?assertMatch(
    [{{els_dap_test_module, 7}, _}, {{els_dap_test_module, 9}, _}],
    els_dap_rpc:all_breaks(Node)
  ),
  els_provider:handle_request(
    Provider,
    request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module)
                           , [9])
  ),
  els_provider:handle_request(
    Provider,
    request_set_function_breakpoints([])
  ),
  ?assertMatch([{{els_dap_test_module, 9}, _}], els_dap_rpc:all_breaks(Node)),
  ok.

-spec project_node_exit(config()) -> ok.
project_node_exit(Config) ->
  NodeName = ?config(node, Config),
  Node = binary_to_atom(NodeName, utf8),
  % ok = meck:new(els_utils, [passthrough]),
  meck:expect(els_utils, halt, 1, meck:val(ok)),
  meck:reset(els_dap_server),
  erlang:monitor_node(Node, true),
  %% kill node and wait for nodedown message
  rpc:cast(Node, erlang, halt, []),
  receive
    {nodedown, Node} -> ok
  end,
  %% wait until els_utils:halt has been called
  els_test_utils:wait_until_mock_called(els_utils, halt).
  %% there is a race condition in CI, important is that the process stops
  % ?assert(meck:called(els_dap_server, send_event, [<<"terminated">>, '_'])),
  % ?assert(meck:called(els_dap_server, send_event, [<<"exited">>, '_'])).

-spec breakpoints_with_cond(config()) -> ok.
breakpoints_with_cond(Config) ->
  breakpoints_base(Config, 9, #{condition => <<"N =:= 5">>}, <<"5">>).

-spec breakpoints_with_hit(config()) -> ok.
breakpoints_with_hit(Config) ->
  breakpoints_base(Config, 9, #{hitcond => <<"3">>}, <<"8">>).

-spec breakpoints_with_cond_and_hit(config()) -> ok.
breakpoints_with_cond_and_hit(Config) ->
  Params = #{condition => <<"N < 7">>, hitcond => <<"3">>},
  breakpoints_base(Config, 9, Params, <<"4">>).

%% Parameterizable base test for breakpoints: sets up a breakpoint with given
%% parameters and checks the value of N when first hit
breakpoints_base(Config, BreakLine, Params, NExp) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, els_dap_test_module, entry, [10])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  meck:reset([els_dap_server]),

  els_provider:handle_request(
    Provider,
    request_set_breakpoints(
      path_to_test_module(DataDir, els_dap_test_module),
      [{BreakLine, Params}]
    )
  ),
  %% hit breakpoint
  els_provider:handle_request(Provider, request_configuration_done(#{})),
  ?assertEqual(ok, wait_for_break(Node, els_dap_test_module, BreakLine)),
  %% check value of N
  #{<<"threads">> := [#{<<"id">> := ThreadId}]} =
    els_provider:handle_request( Provider
                               , request_threads()
                               ),
  #{<<"stackFrames">> := [#{<<"id">> := FrameId}|_]} =
    els_provider:handle_request( Provider
                               , request_stack_frames(ThreadId)
                               ),
  #{<<"scopes">> := [#{<<"variablesReference">> := VariableRef}]} =
    els_provider:handle_request(Provider, request_scope(FrameId)),
  #{<<"variables">> := [NVar]} =
    els_provider:handle_request(Provider, request_variable(VariableRef)),
  ?assertMatch(#{ <<"name">> := <<"N">>
                , <<"value">> := NExp
                , <<"variablesReference">> := 0
                }
              , NVar),
  ok.

-spec log_points(config()) -> ok.
log_points(Config) ->
  log_points_base(Config, 9, #{log => <<"N">>}, 11, 1).

-spec log_points_with_lt_condition(config()) -> ok.
log_points_with_lt_condition(Config) ->
  log_points_base(Config, 9, #{log => <<"N">>, condition => <<"N < 5">>}, 7, 4).

-spec log_points_with_eq_condition(config()) -> ok.
log_points_with_eq_condition(Config) ->
  Params = #{log => <<"N">>, condition => <<"N =:= 5">>},
  log_points_base(Config, 9, Params, 7, 1).

-spec log_points_with_hit(config()) -> ok.
log_points_with_hit(Config) ->
  log_points_base(Config, 9, #{log => <<"N">>, hitcond => <<"3">>}, 7, 3).

-spec log_points_with_hit1(config()) -> ok.
log_points_with_hit1(Config) ->
  log_points_base(Config, 9, #{log => <<"N">>, hitcond => <<"1">>}, 7, 10).

-spec log_points_with_cond_and_hit(config()) -> ok.
log_points_with_cond_and_hit(Config) ->
  Params = #{log => <<"N">>, condition => <<"N < 5">>, hitcond => <<"2">>},
  log_points_base(Config, 9, Params, 7, 2).

-spec log_points_empty_cond(config()) -> ok.
log_points_empty_cond(Config) ->
  log_points_base(Config, 9, #{log => <<"N">>, condition => <<>>}, 11, 1).

%% Parameterizable base test for logpoints: sets up a logpoint with given
%% parameters and checks how many hits it gets before hitting a given breakpoint
log_points_base(Config, LogLine, Params, BreakLine, NumCalls) ->
  Provider = ?config(provider, Config),
  DataDir = ?config(data_dir, Config),
  Node = ?config(node, Config),
  els_provider:handle_request(Provider, request_initialize(#{})),
  els_provider:handle_request(
    Provider,
    request_launch(DataDir, Node, els_dap_test_module, entry, [10])
  ),
  els_test_utils:wait_until_mock_called(els_dap_server, send_event),
  meck:reset([els_dap_server]),

  els_provider:handle_request(
    Provider,
    request_set_breakpoints(
      path_to_test_module(DataDir, els_dap_test_module),
      [{LogLine, Params}, BreakLine]
    )
  ),
  els_provider:handle_request(Provider, request_configuration_done(#{})),
  ?assertEqual(ok, wait_for_break(Node, els_dap_test_module, BreakLine)),
  ?assertEqual(NumCalls,
               meck:num_calls(els_dap_server, send_event, [<<"output">>, '_'])),
  ok.

%%==============================================================================
%% Requests
%%==============================================================================

request_initialize(Params) ->
  {<<"initialize">>, Params}.

request_launch(Params) ->
  {<<"launch">>, Params}.

request_launch(AppDir, Node, M, F, A) ->
  request_launch(
    #{ <<"projectnode">> => Node
     , <<"cwd">> => AppDir
     , <<"module">> => atom_to_binary(M, utf8)
     , <<"function">> => atom_to_binary(F, utf8)
     , <<"args">> => unicode:characters_to_binary(io_lib:format("~w", [A]))
     }).

request_launch(AppDir, Node, Cookie, M, F, A) ->
  {<<"launch">>, Params} = request_launch(AppDir, Node, M, F, A),
  {<<"launch">>, Params#{<<"cookie">> => Cookie}}.

request_launch(AppDir, Node, Cookie, M, F, A, use_long_names) ->
    {<<"launch">>, Params} = request_launch(AppDir, Node, M, F, A),
    {<<"launch">>, Params#{<<"cookie">> => Cookie,
                           <<"use_long_names">> => true}}.

request_configuration_done(Params) ->
  {<<"configurationDone">>, Params}.

request_set_breakpoints(File, Specs) ->
  { <<"setBreakpoints">>
  , #{ <<"source">> => #{<<"path">> => File}
     , <<"sourceModified">> => false
     , <<"breakpoints">> => lists:map(fun map_spec/1, Specs)
     }}.

map_spec({Line, Params}) ->
  Cond = case Params of
    #{condition := CondExpr} -> #{<<"condition">> => CondExpr};
    _ -> #{}
  end,
  Hit = case Params of
    #{hitcond := HitExpr} -> #{<<"hitCondition">> => HitExpr};
    _ -> #{}
  end,
  Log = case Params of
    #{log := LogMsg} -> #{<<"logMessage">> => LogMsg};
    _ -> #{}
  end,
  lists:foldl(fun maps:merge/2, #{<<"line">> => Line}, [Cond, Hit, Log]);
map_spec(Line) -> #{<<"line">> => Line}.

request_set_function_breakpoints(MFAs) ->
  {<<"setFunctionBreakpoints">>, #{
    <<"breakpoints">> => [#{ <<"name">> => MFA
                           , <<"enabled">> => true} || MFA <- MFAs]
  }}.

request_stack_frames(ThreadId) ->
  {<<"stackTrace">>, #{<<"threadId">> => ThreadId}}.

request_scope(FrameId) ->
  {<<"scopes">>, #{<<"frameId">> => FrameId}}.

request_variable(Ref) ->
  {<<"variables">>, #{<<"variablesReference">> => Ref}}.

request_threads() ->
  {<<"threads">>, #{}}.

request_step_in(ThreadId) ->
  {<<"stepIn">>, #{<<"threadId">> => ThreadId}}.

request_next(ThreadId) ->
  {<<"next">>, #{<<"threadId">> => ThreadId}}.

request_continue(ThreadId) ->
  {<<"continue">>, #{<<"threadId">> => ThreadId}}.

request_evaluate(Context, FrameId, Expression) ->
  {<<"evaluate">>,
    #{ <<"context">> => Context
     , <<"frameId">> => FrameId
     , <<"expression">> => Expression
     }
  }.
