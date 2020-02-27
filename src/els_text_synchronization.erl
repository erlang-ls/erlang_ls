-module(els_text_synchronization).

-include("erlang_ls.hrl").

-export([ did_open/1
        , did_save/1
        , did_close/1
        ]).

-export([ generate_diagnostics/1
        , maybe_compile_and_load/2
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  ok           = els_indexer:index(Uri, Text),
  spawn (?MODULE, generate_diagnostics, [Uri]),
  ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  spawn (?MODULE, generate_diagnostics, [Uri]),
  ok.

-spec did_close(map()) -> ok.
did_close(_Params) -> ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec generate_diagnostics(uri()) -> ok.
generate_diagnostics(Uri) ->
  CDiagnostics = els_compiler_diagnostics:diagnostics(Uri),
  DDiagnostics = els_dialyzer_diagnostics:diagnostics(Uri),
  EDiagnostics = els_elvis_diagnostics:diagnostics(Uri),
  Method = <<"textDocument/publishDiagnostics">>,
  Params1  = #{ uri => Uri
              , diagnostics => CDiagnostics ++ DDiagnostics ++ EDiagnostics
              },
  maybe_compile_and_load(Uri, CDiagnostics),
  els_server:send_notification(Method, Params1).

-spec maybe_compile_and_load(uri(), [diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := NodeStr} ->
      Node = list_to_atom(NodeStr),
      Module = els_uri:module(Uri),
      case rpc:call(Node, code, is_sticky, [Module]) of
        true -> ok;
        _ -> handle_rpc_result(rpc:call(Node, c, c, [Module]), Module)
      end;
    disabled ->
      ok
  end;
maybe_compile_and_load(_Uri, _CDiagnostics) ->
  ok.

-spec handle_rpc_result(term() | {badrpc, term()}, atom()) -> ok.
handle_rpc_result({ok, Module}, _) ->
  Msg = io_lib:format("code_reload success for: ~s", [Module]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_INFO,
                                  message => list_to_binary(Msg)
                                });
handle_rpc_result(Err, Module) ->
  lager:info("[code_reload] code_reload using c:c/1 crashed with: ~p",
             [Err]),
  Msg = io_lib:format("code_reload swap crashed for: ~s with: ~w",
                      [Module, Err]),
  els_server:send_notification(<<"window/showMessage">>,
                               #{ type => ?MESSAGE_TYPE_ERROR,
                                  message => list_to_binary(Msg)
                                }).
