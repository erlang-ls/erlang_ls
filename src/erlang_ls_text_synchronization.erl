-module(erlang_ls_text_synchronization).

-export([ did_open/1
        , did_save/2
        , did_close/1
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  Document     = erlang_ls_document:create(Uri, Text),
  ok           = erlang_ls_indexer:index(Document).

-spec did_save(map(), pid()) -> ok.
did_save(Params, Server) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  CDiagnostics = erlang_ls_compiler_diagnostics:diagnostics(Uri),
  DDiagnostics = erlang_ls_dialyzer_diagnostics:diagnostics(Uri),
  Method = <<"textDocument/publishDiagnostics">>,
  Params1  = #{ uri => Uri
              , diagnostics => CDiagnostics ++ DDiagnostics
              },
  erlang_ls_server:send_notification(Server, Method, Params1).

-spec did_close(map()) -> ok.
did_close(_Params) -> ok.
