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
    configuration_done/1,
    configuration_done_with_breakpoint/1,
    frame_variables/1
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
    %% at this point there should be only one variable present
    ?assertMatch(
        #{
            <<"name">> := <<"N">>,
            <<"value">> := <<"5">>,
            <<"variablesReference">> := 0
        },
        NVar
    ),
    ok.

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

request_configuration_done(Params) ->
    {<<"configurationDone">>, Params}.

request_set_breakpoints(File, Lines) ->
    {<<"setBreakpoints">>, #{
        <<"source">> => #{<<"path">> => File},
        <<"sourceModified">> => false,
        <<"breakpoints">> => [#{<<"line">> => Line} || Line <- Lines]
    }}.

request_stack_frames(ThreadId) ->
    {<<"stackTrace">>, #{<<"threadId">> => ThreadId}}.

request_scope(FrameId) ->
    {<<"scopes">>, #{<<"frameId">> => FrameId}}.

request_variable(Ref) ->
    {<<"variables">>, #{<<"variablesReference">> => Ref}}.
