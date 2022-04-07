-module(els_indexing).

-callback index(els_dt_document:item()) -> ok.

%% API
-export([ find_and_deeply_index_file/1
        , index_dir/2
        , start/0
        , maybe_start/0
        , ensure_deeply_indexed/1
        , shallow_index/2
        , deep_index/1
        , remove/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec find_and_deeply_index_file(string()) ->
   {ok, uri()} | {error, any()}.
find_and_deeply_index_file(FileName) ->
  SearchPaths = els_config:get(search_paths),
  case file:path_open( SearchPaths
                     , els_utils:to_binary(FileName)
                     , [read]
                     )
  of
    {ok, IoDevice, Path} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      Uri = els_uri:uri(Path),
      ensure_deeply_indexed(Uri),
      {ok, Uri};
    {error, Error} ->
      {error, Error}
  end.

-spec is_generated_file(binary(), string()) -> boolean().
is_generated_file(Text, Tag) ->
  [Line|_] = string:split(Text, "\n", leading),
  case re:run(Line, Tag) of
    {match, _} ->
      true;
    nomatch ->
      false
  end.

-spec ensure_deeply_indexed(uri()) -> ok.
ensure_deeply_indexed(Uri) ->
  {ok, #{pois := POIs} = Document} = els_utils:lookup_document(Uri),
  case POIs of
    ondemand ->
      deep_index(Document);
    _ ->
      ok
  end.

-spec deep_index(els_dt_document:item()) -> ok.
deep_index(Document) ->
  #{id := Id, uri := Uri, text := Text, source := Source} = Document,
  {ok, POIs} = els_parser:parse(Text),
  ok = els_dt_document:insert(Document#{pois => POIs}),
  index_signatures(Id, Text, POIs),
  case Source of
    otp ->
      ok;
    S when S =:= app orelse S =:= dep ->
      index_references(Id, Uri, POIs)
  end.

-spec index_signatures(atom(), binary(), [poi()]) -> ok.
index_signatures(Id, Text, POIs) ->
  ok = els_dt_signatures:delete_by_module(Id),
  [index_signature(Id, Text, POI) || #{kind := spec} = POI <- POIs],
  ok.

-spec index_signature(atom(), binary(), poi()) -> ok.
index_signature(_M, _Text, #{id := undefined}) ->
  ok;
index_signature(M, Text,   #{id := {F, A}, range := Range}) ->
  #{from := From, to := To} = Range,
  Spec = els_text:range(Text, From, To),
  els_dt_signatures:insert(#{ mfa => {M, F, A}, spec => Spec}).

-spec index_references(atom(), uri(), [poi()]) -> ok.
index_references(Id, Uri, POIs) ->
  ok = els_dt_references:delete_by_uri(Uri),
  ReferenceKinds = [ %% Function
                     application
                   , implicit_fun
                   , import_entry
                     %% Include
                   , include
                   , include_lib
                     %% Behaviour
                   , behaviour
                     %% Type
                   , type_application
                   ],
  [index_reference(Id, Uri, POI)
   || #{kind := Kind} = POI <- POIs,
      lists:member(Kind, ReferenceKinds)],
  ok.

-spec index_reference(atom(), uri(), poi()) -> ok.
index_reference(M, Uri, #{id := {F, A}} = POI) ->
  index_reference(M, Uri, POI#{id => {M, F, A}});
index_reference(_M, Uri, #{kind := Kind, id := Id, range := Range}) ->
  els_dt_references:insert(Kind, #{id => Id, uri => Uri, range => Range}).

-spec shallow_index(binary(), els_dt_document:source()) -> {ok, uri()}.
shallow_index(Path, Source) ->
  Uri = els_uri:uri(Path),
  {ok, Text} = file:read_file(Path),
  shallow_index(Uri, Text, Source),
  {ok, Uri}.

-spec shallow_index(uri(), binary(), els_dt_document:source()) -> ok.
shallow_index(Uri, Text, Source) ->
  Document = els_dt_document:new(Uri, Text, Source),
  ok = els_dt_document:insert(Document),
  #{id := Id, kind := Kind} = Document,
  ModuleItem = els_dt_document_index:new(Id, Uri, Kind),
  ok = els_dt_document_index:insert(ModuleItem).

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
  start(<<"OTP">>, els_config:get(otp_paths), otp),
  start(<<"Applications">>, els_config:get(apps_paths), app),
  start(<<"Dependencies">>, els_config:get(deps_paths), dep).

-spec start(binary(), [string()], els_dt_document:source()) -> ok.
start(Group, Entries, Source) ->
  Skip = els_config_indexing:get_skip_generated_files(),
  SkipTag = els_config_indexing:get_generated_files_tag(),
  Incremental = els_config_indexing:get_incremental(),
  Task = fun(Dir, {Succeeded0, Skipped0, Failed0}) ->
             {Su, Sk, Fa} = index_dir(Dir, Skip, SkipTag, Incremental, Source),
             {Succeeded0 + Su, Skipped0 + Sk, Failed0 + Fa}
         end,
  Config = #{ task => Task
            , entries => Entries
            , title => <<"Indexing ", Group/binary>>
            , initial_state => {0, 0, 0}
            , on_complete =>
                fun({Succeeded, Skipped, Failed}) ->
                    ?LOG_INFO("Completed indexing for ~s "
                              "(succeeded: ~p, skipped: ~p, failed: ~p)",
                              [Group, Succeeded, Skipped, Failed])
                end
            },
  {ok, _Pid} = els_background_job:new(Config),
  ok.

-spec remove(uri()) -> ok.
remove(Uri) ->
  ok = els_dt_document:delete(Uri),
  ok = els_dt_document_index:delete_by_uri(Uri),
  ok = els_dt_references:delete_by_uri(Uri),
  ok = els_dt_signatures:delete_by_module(els_uri:module(Uri)).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec shallow_index(binary(), boolean(), string(), els_dt_document:source()) ->
        ok | skipped.
shallow_index(FullName, SkipGeneratedFiles, GeneratedFilesTag, Source) ->
  Uri = els_uri:uri(FullName),
  ?LOG_DEBUG("Shallow indexing file. [filename=~s] [uri=~s]",
             [FullName, Uri]),
  {ok, Text} = file:read_file(FullName),
  case SkipGeneratedFiles andalso is_generated_file(Text, GeneratedFilesTag) of
    true ->
      ?LOG_DEBUG("Skip indexing for generated file ~p", [Uri]),
      skipped;
    false ->
      shallow_index(Uri, Text, Source)
  end.

-spec deep_index(binary(), boolean(), string(), els_dt_document:source()) ->
        ok | skipped | error.
deep_index(FullName, SkipGeneratedFiles, GeneratedFilesTag, Source) ->
  Uri = els_uri:uri(FullName),
  ?LOG_DEBUG("Deep indexing file. [filename=~s] [uri=~s]",
             [FullName, Uri]),
  {ok, Text} = file:read_file(FullName),
  case SkipGeneratedFiles andalso is_generated_file(Text, GeneratedFilesTag) of
    true ->
      ?LOG_DEBUG("Skip indexing for generated file ~p", [Uri]),
      skipped;
    false ->
      Document = els_dt_document:new(Uri, Text, Source),
      try deep_index(Document)
      catch Type:Reason:St ->
          ?LOG_ERROR("Error indexing file "
                     "[filename=~s, uri=~s] "
                     "~p:~p:~p", [FullName, Uri, Type, Reason, St]),
          failed
      end
  end.

-spec index_dir(string(), els_dt_document:source()) ->
        {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Source) ->
  Skip = els_config_indexing:get_skip_generated_files(),
  SkipTag = els_config_indexing:get_generated_files_tag(),
  Incremental = els_config_indexing:get_incremental(),
  index_dir(Dir, Skip, SkipTag, Incremental, Source).

-spec index_dir(string(), boolean(), string(), boolean(),
                els_dt_document:source()) ->
        {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Skip, SkipTag, Incremental, Source) ->
  ?LOG_DEBUG("Indexing directory. [dir=~s]", [Dir]),
  F = fun(FileName, {Succeeded, Skipped, Failed}) ->
          BinaryName = els_utils:to_binary(FileName),
          Result =
            case Incremental of
              true ->
                shallow_index(BinaryName, Skip, SkipTag, Source);
              false ->
                deep_index(BinaryName, Skip, SkipTag, Source)
            end,
          case Result of
            ok -> {Succeeded + 1, Skipped, Failed};
            skipped -> {Succeeded, Skipped + 1, Failed};
            failed -> {Succeeded, Skipped, Failed}
          end
      end,
  Filter = fun(Path) ->
               Ext = filename:extension(Path),
               lists:member(Ext, [".erl", ".hrl", ".escript"])
           end,

  {Time, {Succeeded, Skipped, Failed}} = timer:tc( els_utils
                                                 , fold_files
                                                 , [ F
                                                   , Filter
                                                   , Dir
                                                   , {0, 0, 0}
                                                   ]
                                                 ),
  ?LOG_DEBUG("Finished indexing directory. [dir=~s] [time=~p] "
             "[succeeded=~p] [skipped=~p] [failed=~p]",
             [Dir, Time/1000/1000, Succeeded, Skipped, Failed]),
  {Succeeded, Skipped, Failed}.
