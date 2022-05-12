-module(els_execute_command_SUITE).

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
    els_lsp_info/1,
    ct_run_test/1,
    strip_server_prefix/1,
    suggest_spec/1
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
init_per_testcase(ct_run_test, Config0) ->
    Config = els_test_utils:init_per_testcase(ct_run_test, Config0),
    setup_mocks(),
    Config;
init_per_testcase(suggest_spec, Config0) ->
    Config = els_test_utils:init_per_testcase(suggest_spec, Config0),
    meck:new(els_protocol, [passthrough, no_link]),
    meck:expect(
        els_protocol,
        request,
        3,
        fun(RequestId, Method, Params) ->
            meck:passthrough([RequestId, Method, Params])
        end
    ),
    Config;
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(ct_run_test, Config) ->
    teardown_mocks(),
    els_test_utils:end_per_testcase(ct_run_test, Config);
end_per_testcase(suggest_spec, Config) ->
    meck:unload(els_protocol),
    els_test_utils:end_per_testcase(ct_run_test, Config);
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec els_lsp_info(config()) -> ok.
els_lsp_info(Config) ->
    Uri = ?config(code_navigation_uri, Config),
    PrefixedCommand = els_command:with_prefix(<<"server-info">>),
    #{result := Result} =
        els_client:workspace_executecommand(PrefixedCommand, [#{uri => Uri}]),
    Expected = [],
    ?assertEqual(Expected, Result),
    Notifications = wait_for_notifications(2),
    [
        begin
            ?assertEqual(maps:get(method, Notification), <<"window/showMessage">>),
            Params = maps:get(params, Notification),
            ?assertEqual(
                <<"Erlang LS (in code_navigation), ">>,
                binary:part(maps:get(message, Params), 0, 32)
            )
        end
     || Notification <- Notifications
    ],
    ok.

-spec ct_run_test(config()) -> ok.
ct_run_test(Config) ->
    Uri = ?config(sample_SUITE_uri, Config),
    PrefixedCommand = els_command:with_prefix(<<"ct-run-test">>),
    #{result := Result} =
        els_client:workspace_executecommand(
            PrefixedCommand,
            [
                #{
                    uri => Uri,
                    module => sample_SUITE,
                    function => one,
                    arity => 1,
                    line => 58
                }
            ]
        ),
    Expected = [],
    ?assertEqual(Expected, Result),
    els_test_utils:wait_until_mock_called(els_protocol, notification),
    ?assertEqual(1, meck:num_calls(els_distribution_server, rpc_call, '_')),
    Notifications = [
        {Method, Args}
     || {_Pid, {els_protocol, notification, [<<"textDocument/publishDiagnostics">> = Method, Args]},
            _Result} <- meck:history(els_protocol)
    ],
    ?assertEqual(
        [
            {<<"textDocument/publishDiagnostics">>, #{
                diagnostics =>
                    [
                        #{
                            message => <<"Test passed!">>,
                            range =>
                                #{
                                    'end' => #{character => 0, line => 58},
                                    start => #{character => 0, line => 57}
                                },
                            severity => 3,
                            source => <<"Common Test">>
                        }
                    ],
                uri => Uri
            }}
        ],
        Notifications
    ),
    ok.

-spec suggest_spec(config()) -> ok.
suggest_spec(Config) ->
    Uri = ?config(execute_command_suggest_spec_uri, Config),
    PrefixedCommand = els_command:with_prefix(<<"suggest-spec">>),
    #{result := Result} =
        els_client:workspace_executecommand(
            PrefixedCommand,
            [
                #{
                    uri => Uri,
                    line => 12,
                    spec => <<
                        "-spec without_spec(number(),binary()) -> "
                        "{number(),binary()}."
                    >>
                }
            ]
        ),
    Expected = [],
    ?assertEqual(Expected, Result),
    Pattern = ['_', <<"workspace/applyEdit">>, '_'],
    ok = meck:wait(1, els_protocol, request, Pattern, 5000),
    History = meck:history(els_protocol),
    [Edit] = [
        Params
     || {_Pid, {els_protocol, request, [_RequestId, <<"workspace/applyEdit">>, Params]}, _Binary} <-
            History
    ],
    #{
        edit := #{
            changes := #{
                Uri := [
                    #{
                        newText := NewText,
                        range := Range
                    }
                ]
            }
        }
    } = Edit,
    ?assertEqual(
        <<
            "-spec without_spec(number(),binary()) -> "
            "{number(),binary()}.\n"
            "without_spec(A, B) when is_binary(B) ->\n"
        >>,
        NewText
    ),
    ?assertEqual(
        #{
            'end' => #{
                character => 0,
                line => 12
            },
            start => #{
                character => 0,
                line => 11
            }
        },
        Range
    ),
    ok.

-spec strip_server_prefix(config()) -> ok.
strip_server_prefix(_Config) ->
    PrefixedCommand = els_command:with_prefix(<<"server-info">>),
    ?assertEqual(
        <<"server-info">>,
        els_command:without_prefix(PrefixedCommand)
    ),

    ?assertEqual(
        <<"server-info">>,
        els_command:without_prefix(<<"123:server-info">>)
    ),

    ?assertEqual(
        <<"server-info">>,
        els_command:without_prefix(<<"server-info">>)
    ),

    ?assertEqual(
        <<"server-info:f">>,
        els_command:without_prefix(<<"13:server-info:f">>)
    ),
    ok.

-spec setup_mocks() -> ok.
setup_mocks() ->
    meck:new(els_protocol, [passthrough, no_link]),
    meck:expect(
        els_distribution_server,
        rpc_call,
        4,
        fun(_, _, _, _) -> {ok, <<"Test passed!">>} end
    ),
    meck:expect(
        els_protocol,
        notification,
        2,
        fun(Method, Params) ->
            meck:passthrough([Method, Params])
        end
    ),
    ok.

-spec teardown_mocks() -> ok.
teardown_mocks() ->
    meck:unload(els_protocol),
    ok.

-spec wait_for_notifications(pos_integer()) -> [map()].
wait_for_notifications(Num) ->
    wait_for_notifications(Num, []).

-spec wait_for_notifications(integer(), [map()]) -> [map()].
wait_for_notifications(Num, Acc) when Num =< 0 ->
    Acc;
wait_for_notifications(Num, Acc) ->
    CheckFun = fun() ->
        case els_client:get_notifications() of
            [] -> false;
            Notifications -> {true, Notifications}
        end
    end,
    {ok, Notifications} = els_test_utils:wait_for_fun(CheckFun, 10, 3),
    wait_for_notifications(Num - length(Notifications), Acc).
