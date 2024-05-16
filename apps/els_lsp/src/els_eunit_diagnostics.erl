-module(els_eunit_diagnostics).
-behaviour(els_diagnostics).

%%% els_diagnostics callbacks
-export([run/1]).
-export([is_default/0]).
-export([source/0]).
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%% els_diagnostics callbacks
-spec is_default() -> boolean().
is_default() ->
    false.

-spec source() -> binary().
source() ->
    <<"EUnit">>.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    case run_eunit_on_remote_node(Uri) of
        ignore ->
            ?LOG_INFO("No remote node to run on."),
            [];
        _Res ->
            receive
                {result, Collected} ->
                    lists:flatmap(
                        fun(Data) ->
                            handle_result(Uri, Data)
                        end,
                        Collected
                    )
            after 5000 ->
                ?LOG_INFO("EUnit Timeout."),
                []
            end
    end.

-spec run_eunit_on_remote_node(uri()) -> ignore | any().
run_eunit_on_remote_node(Uri) ->
    Ext = filename:extension(Uri),
    case els_config:get(code_reload) of
        #{"node" := NodeOrNodes} when Ext == <<".erl">> ->
            Module = els_uri:module(Uri),
            case NodeOrNodes of
                [Node | _] when is_list(Node) ->
                    rpc_eunit(Node, Module);
                Node when is_list(Node) ->
                    rpc_eunit(Node, Module);
                _ ->
                    ignore
            end;
        _ ->
            ignore
    end.

-spec rpc_eunit(string(), module()) -> any().
rpc_eunit(NodeStr, Module) ->
    ?LOG_INFO("Running EUnit tests for ~p on ~s.", [Module, NodeStr]),
    Listener = els_eunit_listener:start([{parent_pid, self()}]),
    rpc:call(
        node_name(NodeStr),
        eunit,
        test,
        [
            Module,
            [
                {report, Listener},
                {exact_execution, true},
                {no_tty, true}
            ]
        ]
    ).

-spec node_name(string()) -> atom().
node_name(N) ->
    els_utils:compose_node_name(N, els_config_runtime:get_name_type()).

-spec handle_result(uri(), any()) -> [els_diagnostics:diagnostic()].
handle_result(Uri, Data) ->
    Status = proplists:get_value(status, Data),
    case Status of
        {error, {error, {Assertion, Info0}, _Stack}} when
            Assertion == assert;
            Assertion == assertNot;
            Assertion == assertMatch;
            Assertion == assertNotMatch;
            Assertion == assertEqual;
            Assertion == assertNotEqual;
            Assertion == assertException;
            Assertion == assertNotException;
            Assertion == assertError;
            Assertion == assertExit;
            Assertion == assertThrow;
            Assertion == assertCmd;
            Assertion == assertCmdOutput
        ->
            Info1 = lists:keydelete(module, 1, Info0),
            {value, {line, Line}, Info2} = lists:keytake(line, 1, Info1),
            Msg =
                io_lib:format("~p failed.\n", [Assertion]) ++
                    [format_info_value(K, V) || {K, V} <- Info2] ++
                    format_output(Data),
            [diagnostic(Line, Msg, ?DIAGNOSTIC_ERROR)];
        ok ->
            Line = get_line(Uri, Data),
            Msg = "Test passed." ++ format_output(Data),
            [diagnostic(Line, Msg, ?DIAGNOSTIC_INFO)];
        {error, {error, Error, Stack}} ->
            UriM = els_uri:module(Uri),
            case [X || {M, _, _, _} = X <- Stack, M == UriM] of
                [] ->
                    %% Current module not in stacktrace
                    %% Error will be placed on line 0
                    Msg = io_lib:format("Test crashed: ~p\n~p", [Error, Stack]),
                    Line = 0,
                    [diagnostic(Line, Msg, ?DIAGNOSTIC_ERROR)];
                [{M, F, A, Info0} | _] ->
                    Msg = io_lib:format("Test crashed: ~p", [Error]),
                    Line = get_line(Uri, [{source, {M, F, A}} | Info0]),
                    [diagnostic(Line, Msg, ?DIAGNOSTIC_ERROR)]
            end;
        Error ->
            Line = proplists:get_value(line, Data),
            Msg = io_lib:format("Test crashed: ~p", [Error]),
            [diagnostic(Line, Msg, ?DIAGNOSTIC_ERROR)]
    end.

-spec get_line(uri(), any()) -> non_neg_integer().
get_line(Uri, Data) ->
    case proplists:get_value(line, Data) of
        0 ->
            {M, F, A} = proplists:get_value(source, Data),
            {ok, Document} = els_utils:lookup_document(Uri),
            UriM = els_uri:module(Uri),
            case UriM == M of
                true ->
                    POIs = els_dt_document:pois(Document, [function]),
                    case [R || #{id := Id, range := R} <- POIs, Id == {F, A}] of
                        [] ->
                            0;
                        [#{from := {Line, _}} | _] ->
                            Line
                    end;
                false ->
                    0
            end;
        Line ->
            Line
    end.

-spec format_output(any()) -> iolist().
format_output(Data) ->
    case proplists:get_value(output, Data) of
        [<<>>] ->
            [];
        [Output] ->
            io_lib:format("\noutput:\n~s", [Output])
    end.

-spec diagnostic(non_neg_integer(), iolist(), els_diagnostics:severity()) ->
    els_diagnostics:diagnostic().
diagnostic(Line, Msg, Severity) ->
    #{
        range => els_protocol:range(#{from => {Line, 1}, to => {Line + 1, 1}}),
        severity => Severity,
        source => source(),
        message => list_to_binary(Msg)
    }.

-spec format_info_value(atom(), any()) -> iolist().
format_info_value(K, V) when is_list(V) ->
    io_lib:format("~p: ~s\n", [K, V]);
format_info_value(K, V) ->
    io_lib:format("~p: ~p\n", [K, V]).
