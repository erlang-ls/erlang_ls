-module(els_dap_general_provider_SUITE).

%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    groups/0,
    all/0
]).

%% Test cases
-export([
    initialize/1,
    launch_mfa/1,
    launch_mfa_with_cookie/1,
    configuration_done/1,
    configuration_done_with_breakpoint/1,
    frame_variables/1,
    navigation_and_frames/1,
    set_variable/1,
    breakpoints/1,
    project_node_exit/1
]).

%% TODO: cleanup after dropping support for OTP 21 and 22
-compile({no_auto_import, [atom_to_binary/1, binary_to_atom/1]}).

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
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) when
    TestCase =:= undefined orelse
        TestCase =:= initialize orelse
        TestCase =:= launch_mfa orelse
        TestCase =:= launch_mfa_with_cookie orelse
        TestCase =:= configuration_done orelse
        TestCase =:= configuration_done_with_breakpoint
->
    {ok, DAPProvider} = els_provider:start_link(els_dap_general_provider),
    els_config:start_link(),
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
    Node = binary_to_atom(NodeName),
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
    Node = binary_to_atom(NodeName),
    Checker = fun() ->
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

-spec atom_to_binary(atom()) -> binary().
atom_to_binary(Atom) ->
    list_to_binary(atom_to_list(Atom)).

-spec binary_to_atom(binary()) -> atom().
binary_to_atom(Binary) ->
    list_to_atom(binary_to_list(Binary)).

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
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    ok.

-spec launch_mfa_with_cookie(config()) -> ok.
launch_mfa_with_cookie(Config) ->
    Provider = ?config(provider, Config),
    DataDir = ?config(data_dir, Config),
    Node = ?config(node, Config),
    els_provider:handle_request(Provider, request_initialize(#{})),
    els_provider:handle_request(
        Provider,
        request_launch(DataDir, Node, <<"some_cookie">>, els_dap_test_module, entry, [])
    ),
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
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
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    els_provider:handle_request(Provider, request_configuration_done(#{})),
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
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),

    els_provider:handle_request(
        Provider,
        request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module), [9, 29])
    ),
    els_provider:handle_request(Provider, request_configuration_done(#{})),
    ?assertEqual(ok, wait_for_break(Node, els_dap_test_module, 9)),
    ok.

-spec frame_variables(config()) -> ok.
frame_variables(Config) ->
    Provider = ?config(provider, Config),
    %% get thread ID from mocked DAP response
    #{
        <<"reason">> := <<"breakpoint">>,
        <<"threadId">> := ThreadId
    } = meck:capture(last, els_dap_server, send_event, [<<"stopped">>, '_'], 2),
    %% get stackframe
    #{<<"stackFrames">> := [#{<<"id">> := FrameId}]} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    %% get scope
    #{
        <<"scopes">> := [
            #{
                <<"variablesReference">> := VariableRef
            }
        ]
    } = els_provider:handle_request(Provider, request_scope(FrameId)),
    %% extract variable
    #{<<"variables">> := [NVar]} = els_provider:handle_request(
        Provider,
        request_variable(VariableRef)
    ),
    %% at this point there should be only one variable present,
    ?assertMatch(
        #{
            <<"name">> := <<"N">>,
            <<"value">> := <<"5">>,
            <<"variablesReference">> := 0
        },
        NVar
    ),
    ok.

-spec navigation_and_frames(config()) -> ok.
navigation_and_frames(Config) ->
    %% test next, stepIn, continue and check aginst expeted stack frames
    Provider = ?config(provider, Config),
    #{<<"threads">> := [#{<<"id">> := ThreadId}]} = els_provider:handle_request(
        Provider,
        request_threads()
    ),
    %% next
    %%, reset meck history, to capture next call
    meck:reset([els_dap_server]),
    els_provider:handle_request(Provider, request_next(ThreadId)),
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    %% check
    #{<<"stackFrames">> := Frames1} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    ?assertMatch([#{<<"line">> := 11, <<"name">> := <<"els_dap_test_module:entry/1">>}], Frames1),
    %% continue
    meck:reset([els_dap_server]),
    els_provider:handle_request(Provider, request_continue(ThreadId)),
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    %% check
    #{<<"stackFrames">> := Frames2} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    ?assertMatch(
        [
            #{<<"line">> := 9, <<"name">> := <<"els_dap_test_module:entry/1">>},
            #{<<"line">> := 11, <<"name">> := <<"els_dap_test_module:entry/1">>}
        ],
        Frames2
    ),
    %% stepIn
    meck:reset([els_dap_server]),
    els_provider:handle_request(Provider, request_step_in(ThreadId)),
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    %% check
    #{<<"stackFrames">> := Frames3} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    ?assertMatch(
        [
            #{
                <<"line">> := 15,
                <<"name">> := <<"els_dap_test_module:ds/0">>
            },
            #{<<"line">> := 9, <<"name">> := <<"els_dap_test_module:entry/1">>},
            #{<<"line">> := 11, <<"name">> := <<"els_dap_test_module:entry/1">>}
        ],
        Frames3
    ),
    ok.

-spec set_variable(config()) -> ok.
set_variable(Config) ->
    Provider = ?config(provider, Config),
    #{<<"threads">> := [#{<<"id">> := ThreadId}]} = els_provider:handle_request(
        Provider,
        request_threads()
    ),
    #{<<"stackFrames">> := [#{<<"id">> := FrameId1}]} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    meck:reset([els_dap_server]),
    Result1 = els_provider:handle_request(
        Provider,
        request_evaluate(<<"repl">>, FrameId1, <<"N=1">>)
    ),
    ?assertEqual(#{<<"result">> => <<"1">>}, Result1),

    %% get variable value through hover evaluate
    els_dap_test_utils:wait_until_mock_called(els_dap_server, send_event),
    #{<<"stackFrames">> := [#{<<"id">> := FrameId2}]} = els_provider:handle_request(
        Provider,
        request_stack_frames(ThreadId)
    ),
    ?assertNotEqual(FrameId1, FrameId2),
    Result2 = els_provider:handle_request(
        Provider,
        request_evaluate(<<"hover">>, FrameId2, <<"N">>)
    ),
    ?assertEqual(#{<<"result">> => <<"1">>}, Result2),
    %% get variable value through scopes
    #{
        <<"scopes">> := [
            #{
                <<"variablesReference">> := VariableRef
            }
        ]
    } = els_provider:handle_request(Provider, request_scope(FrameId2)),
    %% extract variable
    #{<<"variables">> := [NVar]} = els_provider:handle_request(
        Provider,
        request_variable(VariableRef)
    ),
    %% at this point there should be only one variable present
    ?assertMatch(
        #{
            <<"name">> := <<"N">>,
            <<"value">> := <<"1">>,
            <<"variablesReference">> := 0
        },
        NVar
    ),
    ok.

-spec breakpoints(config()) -> ok.
breakpoints(Config) ->
    Provider = ?config(provider, Config),
    NodeName = ?config(node, Config),
    Node = binary_to_atom(NodeName),
    DataDir = ?config(data_dir, Config),
    els_provider:handle_request(
        Provider,
        request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module), [9])
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
        request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module), [])
    ),
    ?assertMatch(
        [{{els_dap_test_module, 7}, _}, {{els_dap_test_module, 9}, _}],
        els_dap_rpc:all_breaks(Node)
    ),
    els_provider:handle_request(
        Provider,
        request_set_breakpoints(path_to_test_module(DataDir, els_dap_test_module), [9])
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
    Node = binary_to_atom(NodeName),
    meck:expect(els_utils, halt, 1, meck:val(ok)),
    meck:reset(els_dap_server),
    erlang:monitor_node(Node, true),
    %% kill node and wait for nodedown message
    rpc:cast(Node, erlang, halt, []),
    receive
        {nodedown, Node} -> ok
    end,
    %% wait until els_utils:halt has been called
    els_dap_test_utils:wait_until_mock_called(els_utils, halt),
    ?assert(meck:called(els_dap_server, send_event, [<<"terminated">>, '_'])),
    ?assert(meck:called(els_dap_server, send_event, [<<"exited">>, '_'])).

%%==============================================================================
%% Requests
%%==============================================================================

request_initialize(Params) ->
    {<<"initialize">>, Params}.

request_launch(Params) ->
    {<<"launch">>, Params}.

request_launch(AppDir, Node, M, F, A) ->
    request_launch(#{
        <<"projectnode">> => Node,
        <<"cwd">> => AppDir,
        <<"module">> => atom_to_binary(M),
        <<"function">> => atom_to_binary(F),
        <<"args">> => unicode:characters_to_binary(io_lib:format("~w", [A]))
    }).

request_launch(AppDir, Node, Cookie, M, F, A) ->
    {<<"launch">>, Params} = request_launch(AppDir, Node, M, F, A),
    {<<"launch">>, Params#{<<"cookie">> => Cookie}}.

request_configuration_done(Params) ->
    {<<"configurationDone">>, Params}.

request_set_breakpoints(File, Lines) ->
    {<<"setBreakpoints">>, #{
        <<"source">> => #{<<"path">> => File},
        <<"sourceModified">> => false,
        <<"breakpoints">> => [#{<<"line">> => Line} || Line <- Lines]
    }}.

request_set_function_breakpoints(MFAs) ->
    {<<"setFunctionBreakpoints">>, #{
        <<"breakpoints">> => [#{<<"name">> => MFA, <<"enabled">> => true} || MFA <- MFAs]
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
    {<<"evaluate">>, #{
        <<"context">> => Context,
        <<"frameId">> => FrameId,
        <<"expression">> => Expression
    }}.
