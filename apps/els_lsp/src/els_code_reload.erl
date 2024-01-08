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
        #{"node" := NodeStr} when Ext == <<".erl">> ->
            Nodes =
                case NodeStr of
                    [List | _] when is_list(List) ->
                        NodeStr;
                    List when is_list(List) ->
                        [NodeStr];
                    _ ->
                        not_a_list
                end,
            Module = els_uri:module(Uri),
            [
                begin
                    Node = els_utils:compose_node_name(
                        N,
                        els_config_runtime:get_name_type()
                    ),
                    case rpc:call(Node, code, is_sticky, [Module]) of
                        true -> ok;
                        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
                    end
                end
             || N <- Nodes
            ],
            ok;
        _ ->
            ok
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
