-module(els_indexing).

-callback index(els_dt_document:item()) -> ok.

%% API
-export([
    find_and_deeply_index_file/1,
    index_dir/2,
    start/0,
    maybe_start/0,
    ensure_deeply_indexed/1,
    shallow_index/2,
    shallow_index/3,
    deep_index/1,
    remove/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type version() :: null | integer().

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec find_and_deeply_index_file(string()) ->
    {ok, uri()} | {error, any()}.
find_and_deeply_index_file(FileName) ->
    case get({?MODULE, find_and_deep_index_file, FileName}) of
        undefined ->
            SearchPaths = els_config:get(search_paths),
            case
                file:path_open(
                    SearchPaths,
                    els_utils:to_binary(FileName),
                    [read]
                )
            of
                {ok, IoDevice, Path} ->
                    %% TODO: Avoid opening file twice
                    file:close(IoDevice),
                    Uri = els_uri:uri(Path),
                    ensure_deeply_indexed(Uri),
                    {ok, Uri};
                {error, _} = Error ->
                    %% Optimization!
                    %% If we can't find the file, then memoize the error result
                    %% by storing it in the process dictionary as we most
                    %% likely won't find it if we try again.
                    put({?MODULE, find_and_deeply_index_file, FileName}, Error),
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec is_generated_file(binary(), string()) -> boolean().
is_generated_file(Text, Tag) ->
    [Line | _] = string:split(Text, "\n", leading),
    case re:run(Line, Tag) of
        {match, _} ->
            true;
        nomatch ->
            false
    end.

-spec ensure_deeply_indexed(uri()) -> els_dt_document:item().
ensure_deeply_indexed(Uri) ->
    {ok, #{pois := POIs} = Document} = els_utils:lookup_document(Uri),
    case POIs of
        ondemand ->
            deep_index(Document);
        _ ->
            Document
    end.

-spec deep_index(els_dt_document:item()) -> els_dt_document:item().
deep_index(Document0) ->
    #{
        id := Id,
        uri := Uri,
        text := Text,
        source := Source,
        version := Version
    } = Document0,
    {ok, POIs} = els_parser:parse(Text),
    Words = els_dt_document:get_words(Text),
    Document = Document0#{pois => POIs, words => Words},
    case els_dt_document:versioned_insert(Document) of
        ok ->
            index_signatures(Id, Uri, Text, POIs, Version),
            case Source of
                otp ->
                    ok;
                S when S =:= app orelse S =:= dep ->
                    index_references(Id, Uri, POIs, Version)
            end;
        {error, condition_not_satisfied} ->
            ?LOG_DEBUG("Skip indexing old version [uri=~p]", [Uri]),
            ok
    end,
    Document.

-spec index_signatures(atom(), uri(), binary(), [els_poi:poi()], version()) -> ok.
index_signatures(Id, Uri, Text, POIs, Version) ->
    ok = els_dt_signatures:versioned_delete_by_uri(Uri, Version),
    [index_signature(Id, Text, POI, Version) || #{kind := spec} = POI <- POIs],
    ok.

-spec index_signature(atom(), binary(), els_poi:poi(), version()) -> ok.
index_signature(_M, _Text, #{id := undefined}, _Version) ->
    ok;
index_signature(M, Text, #{id := {F, A}, range := Range}, Version) ->
    #{from := From, to := To} = Range,
    Spec = els_text:range(Text, From, To),
    els_dt_signatures:versioned_insert(#{
        mfa => {M, F, A},
        spec => Spec,
        version => Version
    }).

-spec index_references(atom(), uri(), [els_poi:poi()], version()) -> ok.
index_references(Id, Uri, POIs, Version) ->
    ok = els_dt_references:versioned_delete_by_uri(Uri, Version),
    %% Function
    ReferenceKinds = [
        application,
        implicit_fun,
        import_entry,
        %% Include
        include,
        include_lib,
        %% Macro
        macro,
        %% Record
        record_expr,
        record_field,
        %% Behaviour
        behaviour,
        %% Type
        type_application
    ],
    [
        index_reference(Id, Uri, POI, Version)
     || #{kind := Kind} = POI <- POIs,
        lists:member(Kind, ReferenceKinds)
    ],
    ok.

-spec index_reference(atom(), uri(), els_poi:poi(), version()) -> ok.
index_reference(M, Uri, #{kind := function, id := {F, A}} = POI, Version) ->
    index_reference(M, Uri, POI#{id => {M, F, A}}, Version);
index_reference(_M, Uri, #{kind := Kind, id := Id, range := Range}, Version) ->
    els_dt_references:versioned_insert(Kind, #{
        id => Id,
        uri => Uri,
        range => Range,
        version => Version
    }).

-spec shallow_index(binary(), els_dt_document:source()) -> {ok, uri()}.
shallow_index(Path, Source) ->
    Uri = els_uri:uri(Path),
    {ok, Text} = file:read_file(Path),
    shallow_index(Uri, Text, Source),
    {ok, Uri}.

-spec shallow_index(uri(), binary(), els_dt_document:source()) -> ok.
shallow_index(Uri, Text, Source) ->
    Document = els_dt_document:new(Uri, Text, Source),
    case els_dt_document:versioned_insert(Document) of
        ok ->
            #{id := Id, kind := Kind} = Document,
            ModuleItem = els_dt_document_index:new(Id, Uri, Kind),
            ok = els_dt_document_index:insert(ModuleItem);
        {error, condition_not_satisfied} ->
            ?LOG_DEBUG("Skip indexing old version [uri=~p]", [Uri]),
            ok
    end.

-spec maybe_start() -> true | false.
maybe_start() ->
    IndexingEnabled = els_config:get(indexing_enabled),
    case IndexingEnabled of
        true ->
            start();
        false ->
            ?LOG_INFO("Skipping Indexing (disabled via InitOptions)")
    end,
    IndexingEnabled.

-spec start() -> ok.
start() ->
    Skip = els_config_indexing:get_skip_generated_files(),
    SkipTag = els_config_indexing:get_generated_files_tag(),
    ?LOG_INFO("Start indexing. [skip=~p] [skip_tag=~p]", [Skip, SkipTag]),
    start(<<"OTP">>, Skip, SkipTag, els_config:get(otp_paths), otp),
    start(<<"Applications">>, Skip, SkipTag, els_config:get(apps_paths), app),
    start(<<"Dependencies">>, Skip, SkipTag, els_config:get(deps_paths), dep).

-spec start(
    binary(),
    boolean(),
    string(),
    [string()],
    els_dt_document:source()
) -> ok.
start(Group, Skip, SkipTag, Entries, Source) ->
    Task = fun(Dir, {Succeeded0, Skipped0, Failed0}) ->
        {Su, Sk, Fa} = index_dir(Dir, Skip, SkipTag, Source),
        {Succeeded0 + Su, Skipped0 + Sk, Failed0 + Fa}
    end,
    Start = erlang:monotonic_time(millisecond),
    Config = #{
        task => Task,
        entries => Entries,
        title => <<"Indexing ", Group/binary>>,
        initial_state => {0, 0, 0},
        on_complete =>
            fun({Succeeded, Skipped, Failed}) ->
                End = erlang:monotonic_time(millisecond),
                Duration = End - Start,
                Event = #{
                    group => Group,
                    duration_ms => Duration,
                    succeeded => Succeeded,
                    skipped => Skipped,
                    failed => Failed,
                    type => <<"indexing">>
                },
                ?LOG_INFO(
                    "Completed indexing for ~s "
                    "(succeeded: ~p, skipped: ~p, failed: ~p)",
                    [Group, Succeeded, Skipped, Failed]
                ),
                els_telemetry:send_notification(Event)
            end
    },
    {ok, _Pid} = els_background_job:new(Config),
    ok.

-spec remove(uri()) -> ok.
remove(Uri) ->
    ok = els_dt_document:delete(Uri),
    ok = els_dt_document_index:delete_by_uri(Uri),
    ok = els_dt_references:delete_by_uri(Uri),
    ok = els_dt_signatures:delete_by_uri(Uri).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec shallow_index(binary(), boolean(), string(), els_dt_document:source()) ->
    ok | skipped.
shallow_index(FullName, SkipGeneratedFiles, GeneratedFilesTag, Source) ->
    Uri = els_uri:uri(FullName),
    ?LOG_DEBUG(
        "Shallow indexing file. [filename=~s] [uri=~s]",
        [FullName, Uri]
    ),
    {ok, Text} = file:read_file(FullName),
    case SkipGeneratedFiles andalso is_generated_file(Text, GeneratedFilesTag) of
        true ->
            ?LOG_DEBUG("Skip indexing for generated file ~p", [Uri]),
            skipped;
        false ->
            shallow_index(Uri, Text, Source)
    end.

-spec index_dir(string(), els_dt_document:source()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Source) ->
    Skip = els_config_indexing:get_skip_generated_files(),
    SkipTag = els_config_indexing:get_generated_files_tag(),
    index_dir(Dir, Skip, SkipTag, Source).

-spec index_dir(string(), boolean(), string(), els_dt_document:source()) ->
    {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Skip, SkipTag, Source) ->
    ?LOG_DEBUG("Indexing directory. [dir=~s]", [Dir]),
    F = fun(FileName, {Succeeded, Skipped, Failed}) ->
        BinaryName = els_utils:to_binary(FileName),
        case shallow_index(BinaryName, Skip, SkipTag, Source) of
            ok -> {Succeeded + 1, Skipped, Failed};
            skipped -> {Succeeded, Skipped + 1, Failed}
        end
    end,
    Filter = fun(Path) ->
        Ext = filename:extension(Path),
        lists:member(Ext, [".erl", ".hrl", ".escript"])
    end,

    {Time, {Succeeded, Skipped, Failed}} = timer:tc(
        els_utils,
        fold_files,
        [
            F,
            Filter,
            Dir,
            {0, 0, 0}
        ]
    ),
    ?LOG_DEBUG(
        "Finished indexing directory. [dir=~s] [time=~p] "
        "[succeeded=~p] [skipped=~p] [failed=~p]",
        [Dir, Time / 1000 / 1000, Succeeded, Skipped, Failed]
    ),
    {Succeeded, Skipped, Failed}.
