-module(els_text_synchronization).

-include("els_lsp.hrl").

-export([ did_open/1
        , did_save/1
        , did_close/1
        ]).

-spec did_open(map()) -> ok.
did_open(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  Text         = maps:get(<<"text">>        , TextDocument),
  ok           = els_indexing:index(Uri, Text, 'deep'),
  Provider = els_diagnostics_provider,
  els_provider:handle_request(Provider, {run_diagnostics, Params}),
  ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
  Provider = els_diagnostics_provider,
  els_provider:handle_request(Provider, {run_diagnostics, Params}),
  ok.

-spec did_close(map()) -> ok.
did_close(_Params) -> ok.
