-module(els_text_synchronization).

-include_lib("kernel/include/logger.hrl").
-include("els_lsp.hrl").

-export([
    sync_mode/0,
    did_change/1,
    did_open/1,
    did_save/1,
    did_close/1,
    did_change_watched_files/1
]).

-spec sync_mode() -> text_document_sync_kind().
sync_mode() ->
    case els_config:get(incremental_sync) of
        true -> ?TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL;
        false -> ?TEXT_DOCUMENT_SYNC_KIND_FULL
    end.

-spec did_change(map()) -> ok | {ok, pid()}.
did_change(Params) ->
    ContentChanges = maps:get(<<"contentChanges">>, Params),
    TextDocument = maps:get(<<"textDocument">>, Params),
    Uri = maps:get(<<"uri">>, TextDocument),
    Version = maps:get(<<"version">>, TextDocument),
    case ContentChanges of
        [] ->
            ok;
        [Change] when not is_map_key(<<"range">>, Change) ->
            #{<<"text">> := Text} = Change,
            {ok, Document} = els_utils:lookup_document(Uri),
            NewDocument = Document#{text => Text, version => Version},
            els_dt_document:insert(NewDocument),
            background_index(NewDocument);
        _ ->
            ?LOG_DEBUG("didChange INCREMENTAL [changes: ~p]", [ContentChanges]),
            Edits = [to_edit(Change) || Change <- ContentChanges],
            {ok, #{text := Text0} = Document} = els_utils:lookup_document(Uri),
            Text = els_text:apply_edits(Text0, Edits),
            NewDocument = Document#{text => Text, version => Version},
            els_dt_document:insert(NewDocument),
            background_index(NewDocument)
    end.

-spec did_open(map()) -> ok.
did_open(Params) ->
    #{
        <<"textDocument">> := #{
            <<"uri">> := Uri,
            <<"text">> := Text,
            <<"version">> := Version
        }
    } = Params,
    Document = els_dt_document:new(Uri, Text, _Source = app, Version),
    els_dt_document:insert(Document),
    els_indexing:deep_index(Document),
    ok.

-spec did_save(map()) -> ok.
did_save(Params) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    els_docs_memo:delete_by_uri(Uri),
    ok.

-spec did_change_watched_files(map()) -> ok.
did_change_watched_files(Params) ->
    #{<<"changes">> := Changes} = Params,
    [
        handle_file_change(Uri, Type)
     || #{<<"uri">> := Uri, <<"type">> := Type} <- Changes
    ],
    ok.

-spec did_close(map()) -> ok.
did_close(_Params) -> ok.

-spec to_edit(map()) -> els_text:edit().
to_edit(#{<<"text">> := Text, <<"range">> := Range}) ->
    #{
        <<"start">> := #{<<"character">> := FromC, <<"line">> := FromL},
        <<"end">> := #{<<"character">> := ToC, <<"line">> := ToL}
    } = Range,
    {
        #{
            from => {FromL, FromC},
            to => {ToL, ToC}
        },
        els_utils:to_list(Text)
    }.

-spec handle_file_change(uri(), file_change_type()) -> ok.
handle_file_change(Uri, Type) when
    Type =:= ?FILE_CHANGE_TYPE_CREATED;
    Type =:= ?FILE_CHANGE_TYPE_CHANGED
->
    els_docs_memo:delete_by_uri(Uri),
    reload_from_disk(Uri);
handle_file_change(Uri, Type) when Type =:= ?FILE_CHANGE_TYPE_DELETED ->
    els_docs_memo:delete_by_uri(Uri),
    els_indexing:remove(Uri).

-spec reload_from_disk(uri()) -> ok.
reload_from_disk(Uri) ->
    Path = els_uri:path(Uri),
    case file:read_file(Path) of
        {ok, Text} ->
            els_indexing:shallow_index(Uri, Text, app);
        {error, Error} ->
            %% File is not accessible. This can happen, for example,
            %% during a rebase operation, when the file "appears and
            %% disappears" multiple times in a very short
            %% timeframe. Just log the fact as a warning, but keep
            %% going. It should be possible to buffer
            %% 'didChangeWatchedFiles' requests, but it's not a big
            %% deal.
            ?LOG_WARNING(
                "Error while reloading from disk. Ignoring. "
                "[uri=~p] [error=~p]",
                [
                    Uri, Error
                ]
            )
    end,
    ok.

-spec background_index(els_dt_document:item()) -> {ok, pid()}.
background_index(#{uri := Uri} = Document) ->
    Config = #{
        task => fun(Doc, _State) ->
            els_indexing:deep_index(Doc),
            ok
        end,
        entries => [Document],
        title => <<"Indexing ", Uri/binary>>
    },
    els_background_job:new(Config).
