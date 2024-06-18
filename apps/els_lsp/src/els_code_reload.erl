-module(els_code_reload).

-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-export([
    maybe_compile_and_load/1
]).

-spec maybe_compile_and_load(uri()) -> ok.
maybe_compile_and_load(Uri) ->
    Ext = filename:extension(Uri),
    case els_config:get(code_reload) of
        #{"node" := NodeOrNodes} when Ext == <<".erl">> ->
            Nodes = get_nodes(NodeOrNodes),
            Module = els_uri:module(Uri),
            [rpc_code_reload(Node, Module) || Node <- Nodes],
            ok;
        _ ->
            ok
    end.

-spec rpc_code_reload(atom(), module()) -> ok.
rpc_code_reload(Node, Module) ->
    case rpc:call(Node, code, is_sticky, [Module]) of
        true ->
            ok;
        _ ->
            Options = options(Node, Module),
            ?LOG_INFO(
                "[code_reload] code_reload ~p on ~p with ~p",
                [Module, Node, Options]
            ),
            handle_rpc_result(
                rpc:call(Node, c, c, [Module, Options]), Module
            )
    end.

-spec get_nodes([string()] | string()) -> [atom()].
get_nodes(NodeOrNodes) ->
    Type = els_config_runtime:get_name_type(),
    case NodeOrNodes of
        [Str | _] = Nodes when is_list(Str) ->
            [els_utils:compose_node_name(Name, Type) || Name <- Nodes];
        Name when is_list(Name) ->
            [els_utils:compose_node_name(Name, Type)];
        _ ->
            []
    end.

-spec options(atom(), module()) -> [any()].
options(Node, Module) ->
    case rpc:call(Node, erlang, get_module_info, [Module]) of
        Info when is_list(Info) ->
            CompileInfo = proplists:get_value(compile, Info, []),
            CompileOptions = proplists:get_value(
                options, CompileInfo, []
            ),
            case [Option || {d, 'TEST', _} = Option <- CompileOptions] of
                [] ->
                    %% Ensure TEST define is set, this is to
                    %% enable eunit diagnostics to run
                    [{d, 'TEST', true}];
                _ ->
                    []
            end;
        _ ->
            []
    end.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
    Msg = io_lib:format("code_reload success for: ~s", [Module]),
    els_server:send_notification(
        <<"window/showMessage">>,
        #{
            type => ?MESSAGE_TYPE_INFO,
            message => els_utils:to_binary(Msg)
        }
    );
handle_rpc_result(Err, Module) ->
    ?LOG_INFO(
        "[code_reload] code_reload using c:c/1 crashed with: ~p",
        [Err]
    ),
    Msg = io_lib:format(
        "code_reload swap crashed for: ~s with: ~w",
        [Module, Err]
    ),
    els_server:send_notification(
        <<"window/showMessage">>,
        #{
            type => ?MESSAGE_TYPE_ERROR,
            message => els_utils:to_binary(Msg)
        }
    ).
