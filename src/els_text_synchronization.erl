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
  ok           = els_indexer:index(Uri, Text),
  els_diagnostics_server:on_open(Uri),
  ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  els_diagnostics_server:on_save(Uri),
  ok.

-spec did_close(map()) -> ok.
did_close(_Params) ->
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================
