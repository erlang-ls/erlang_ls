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
  ok           = erlang_ls_index:index(Document).

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
  %% TODO: prune/re-index on save/change
  gen_server:cast(Server, {notification, Method, Params1}).

-spec did_close(map()) -> ok.
did_close(Params) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  case erlang_ls_db:find(documents, Uri) of
    not_found ->
      lager:debug("[SERVER] Attempting to close un-opened text document, ignoring [uri=~p]", [Uri]);
    {ok, _} ->
      %% TODO: Do not delete once DB is persistent
      ok = erlang_ls_db:delete(documents, Uri)
  end,
  ok.
