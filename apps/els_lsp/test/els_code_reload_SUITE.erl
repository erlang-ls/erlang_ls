-module(els_code_reload_SUITE).

%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    code_reload/1,
    code_reload_sticky_mod/1
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
    els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
    mock_rpc(),
    mock_code_reload_enabled(),
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    unmock_rpc(),
    unmock_code_reload_enabled(),
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec code_reload(config()) -> ok.
code_reload(Config) ->
    Uri = ?config(diagnostics_uri, Config),
    Module = els_uri:module(Uri),
    ok = els_code_reload:maybe_compile_and_load(Uri),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    ?assert(meck:called(rpc, call, [NodeName, c, c, [Module, []]])),
    ok.

-spec code_reload_sticky_mod(config()) -> ok.
code_reload_sticky_mod(Config) ->
    Uri = ?config(diagnostics_uri, Config),
    Module = els_uri:module(Uri),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    meck:expect(
        rpc,
        call,
        fun
            (PNode, code, is_sticky, [_]) when PNode =:= NodeName ->
                true;
            (Node, Mod, Fun, Args) ->
                meck:passthrough([Node, Mod, Fun, Args])
        end
    ),
    ok = els_code_reload:maybe_compile_and_load(Uri),
    ?assert(meck:called(rpc, call, [NodeName, code, is_sticky, [Module]])),
    ?assertNot(meck:called(rpc, call, [NodeName, c, c, [Module, []]])),
    ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

mock_rpc() ->
    meck:new(rpc, [passthrough, no_link, unstick]),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    meck:expect(
        rpc,
        call,
        fun
            (PNode, c, c, [Module, '_']) when PNode =:= NodeName ->
                {ok, Module};
            (Node, Mod, Fun, Args) ->
                meck:passthrough([Node, Mod, Fun, Args])
        end
    ).

unmock_rpc() ->
    meck:unload(rpc).

mock_code_reload_enabled() ->
    meck:new(els_config, [passthrough, no_link]),
    meck:expect(
        els_config,
        get,
        fun
            (code_reload) ->
                {ok, HostName} = inet:gethostname(),
                #{"node" => "fakenode@" ++ HostName};
            (Key) ->
                meck:passthrough([Key])
        end
    ).

unmock_code_reload_enabled() ->
    meck:unload(els_config).
