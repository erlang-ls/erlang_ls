-module(els_text_synchronization).

-include("erlang_ls.hrl").

-export([ did_open/1
        , did_save/1
        , did_close/1
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  Document     = els_dt_document:new(Uri, Text),
  ok           = els_indexer:index(Document),
  generate_diagnostics(Uri).

-spec did_save(map()) -> ok.
did_save(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  generate_diagnostics(Uri).

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
  els_server:send_notification(Method, Params1).
