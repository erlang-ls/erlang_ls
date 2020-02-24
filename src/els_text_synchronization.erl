-module(els_text_synchronization).

-include("erlang_ls.hrl").

-export([ did_open/1
        , did_save/1
        , did_close/1
        ]).

-export([ generate_diagnostics/1
        , diagnostics/3
        , maybe_compile_and_load/2
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  Document     = els_dt_document:new(Uri, Text),
  ok           = els_indexer:index(Document),
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
  Sync        = [els_compiler_diagnostics],
  Diagnostics = diagnostics(Uri, Sync, []),
  maybe_compile_and_load(Uri, Diagnostics),

  Async = [ els_dialyzer_diagnostics
          , els_elvis_diagnostics
          ],
  erlang:spawn(?MODULE, diagnostics, [Uri, Async, Diagnostics]),
  ok.

-spec diagnostics(uri(), [module()], [diagnostic()]) -> [diagnostic()].
diagnostics(Uri, Modules, Previous) ->
  Diagnostics = [apply(M, diagnostics, [Uri]) || M <- Modules],
  AllDiagnostics = lists:append([Previous | Diagnostics]),
  Method = <<"textDocument/publishDiagnostics">>,
  Params  = #{ uri         => Uri
             , diagnostics => AllDiagnostics
             },
  els_server:send_notification(Method, Params),
  AllDiagnostics.

-spec maybe_compile_and_load(uri(), [diagnostic()]) -> ok.
maybe_compile_and_load(Uri, [] = _CDiagnostics) ->
  case els_config:get(code_reload) of
    #{"node" := Node} ->
      Module = els_uri:module(Uri),
      handle_rpc_result(rpc:call(list_to_atom(Node), c, c, [Module]), Module);
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
