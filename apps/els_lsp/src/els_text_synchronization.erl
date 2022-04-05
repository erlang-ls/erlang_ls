-module(els_text_synchronization).

-include_lib("kernel/include/logger.hrl").
-include("els_lsp.hrl").

-export([ sync_mode/0
        , did_change/1
        , did_open/1
        , did_save/1
        , did_close/1
        , did_change_watched_files/1
        ]).

-spec sync_mode() -> text_document_sync_kind().
sync_mode() ->
  case els_config:get(incremental_sync) of
    true  -> ?TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL;
    false -> ?TEXT_DOCUMENT_SYNC_KIND_FULL
  end.

-spec did_change(map()) -> ok.
did_change(Params) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    [] ->
      ok;
    [Change] when not is_map_key(<<"range">>, Change) ->
      %% Full text sync
      #{<<"text">> := Text} = Change,
      {Duration, ok} =
        timer:tc(fun() ->
                     {ok, Document} = els_utils:lookup_document(Uri),
                     els_indexing:deep_index(Document)
                 end),
      ?LOG_DEBUG("didChange FULLSYNC [size: ~p] [duration: ~pms]\n",
                 [size(Text), Duration div 1000]),
      ok;
    ContentChanges ->
      %% Incremental sync
      ?LOG_DEBUG("didChange INCREMENTAL [changes: ~p]", [ContentChanges]),
      Edits = [to_edit(Change) || Change <- ContentChanges],
      {Duration, ok} =
        timer:tc(fun() -> els_index_buffer:apply_edits_async(Uri, Edits) end),
      ?LOG_DEBUG("didChange INCREMENTAL [duration: ~pms]\n",
                 [Duration div 1000]),
      ok
  end.

-spec did_open(map()) -> ok.
did_open(Params) ->
  #{<<"textDocument">> := #{ <<"uri">> := Uri
                           , <<"text">> := Text}} = Params,
  ok = els_index_buffer:load(Uri, Text),
  ok = els_index_buffer:flush(Uri),
  Provider = els_diagnostics_provider,
  els_provider:handle_request(Provider, {run_diagnostics, Params}),
  ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
  #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
  {ok, Text} = file:read_file(els_uri:path(Uri)),
  ok = els_index_buffer:load(Uri, Text),
  ok = els_index_buffer:flush(Uri),
  Provider = els_diagnostics_provider,
  els_provider:handle_request(Provider, {run_diagnostics, Params}),
  ok.

-spec did_change_watched_files(map()) -> ok.
did_change_watched_files(Params) ->
  #{<<"changes">> := Changes} = Params,
  [handle_file_change(Uri, Type)
   || #{<<"uri">> := Uri, <<"type">> := Type} <- Changes],
  ok.

-spec did_close(map()) -> ok.
did_close(_Params) -> ok.

-spec to_edit(map()) -> els_text:edit().
to_edit(#{<<"text">> := Text, <<"range">> := Range}) ->
  #{ <<"start">> := #{<<"character">> := FromC, <<"line">> := FromL}
   , <<"end">> := #{<<"character">> := ToC, <<"line">> := ToL}
   } = Range,
  {#{ from => {FromL, FromC}
    , to => {ToL, ToC}
    }, els_utils:to_list(Text)}.

-spec handle_file_change(uri(), file_change_type()) -> ok.
handle_file_change(Uri, Type) when Type =:= ?FILE_CHANGE_TYPE_CREATED;
                                   Type =:= ?FILE_CHANGE_TYPE_CHANGED ->
  {ok, Text} = file:read_file(els_uri:path(Uri)),
  ok = els_index_buffer:load(Uri, Text),
  ok = els_index_buffer:flush(Uri);
handle_file_change(Uri, Type) when Type =:= ?FILE_CHANGE_TYPE_DELETED ->
  els_indexing:remove(Uri).
