-module(els_indexing).

-callback index(els_dt_document:item()) -> ok.

%% API
-export([ find_and_index_file/1
        , index_file/1
        , index/3
        , index_dir/2
        , start/0
        , maybe_start/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type mode()  :: 'deep' | 'shallow'.

%%==============================================================================
%% Exported functions
%%==============================================================================

-spec find_and_index_file(string()) ->
   {ok, uri()} | {error, any()}.
find_and_index_file(FileName) ->
  SearchPaths = els_config:get(search_paths),
  case file:path_open( SearchPaths
                     , els_utils:to_binary(FileName)
                     , [read]
                     )
  of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      index_file(FullName);
    {error, Error} ->
      {error, Error}
  end.

-spec index_file(binary()) -> {ok, uri()}.
index_file(Path) ->
  GeneratedFilesTag = els_config_indexing:get_generated_files_tag(),
  try_index_file(Path, 'deep', false, GeneratedFilesTag),
  {ok, els_uri:uri(Path)}.

-spec index_if_not_generated(uri(), binary(), mode(), boolean(), string()) ->
        ok | skipped.
index_if_not_generated(Uri, Text, Mode, false, _GeneratedFilesTag) ->
  index(Uri, Text, Mode);
index_if_not_generated(Uri, Text, Mode, true, GeneratedFilesTag) ->
  case is_generated_file(Text, GeneratedFilesTag) of
    true ->
      ?LOG_DEBUG("Skip indexing for generated file ~p", [Uri]),
      skipped;
    false ->
      ok = index(Uri, Text, Mode)
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

-spec index(uri(), binary(), mode()) -> ok.
index(Uri, Text, Mode) ->
  MD5 = erlang:md5(Text),
  case els_dt_document:lookup(Uri) of
    {ok, [#{md5 := MD5}]} ->
      ok;
    {ok, LookupResult} ->
      Document = els_dt_document:new(Uri, Text),
      do_index(Document, Mode, LookupResult =/= [])
  end.

-spec do_index(els_dt_document:item(), mode(), boolean()) -> ok.
do_index(#{uri := Uri, id := Id, kind := Kind} = Document, Mode, Reset) ->
  case Mode of
    'deep' ->
      ok = els_dt_document:insert(Document);
    'shallow' ->
      %% Don't store detailed POIs when "shallow" indexing.
      %% They will be reloaded and inserted when needed
      %% by calling els_utils:lookup_document/1
      ok
  end,
  %% Mapping from document id to uri
  ModuleItem = els_dt_document_index:new(Id, Uri, Kind),
  ok = els_dt_document_index:insert(ModuleItem),
  index_signatures(Document),
  index_references(Document, Mode, Reset).

-spec index_signatures(els_dt_document:item()) -> ok.
index_signatures(#{id := Id, text := Text} = Document) ->
  Specs  = els_dt_document:pois(Document, [spec]),
  [ begin
      #{from := From, to := To} = Range,
      Spec = els_text:range(Text, From, To),
      els_dt_signatures:insert(#{ mfa => {Id, F, A} , spec => Spec})
    end
    || #{id := {F, A}, range := Range} <- Specs
  ],
  ok.

-spec index_references(els_dt_document:item(), mode(), boolean()) -> ok.
index_references(#{uri := Uri} = Document, 'deep', true) ->
  %% Optimization to only do (non-optimized) match_delete when necessary
  ok = els_dt_references:delete_by_uri(Uri),
  index_references(Document, 'deep', false);
index_references(#{uri := Uri} = Document, 'deep', false) ->
  %% References
  POIs  = els_dt_document:pois(Document, [ application
                                         , behaviour
                                         , implicit_fun
                                         , include
                                         , include_lib
                                         , type_application
                                         , import_entry
                                         ]),
  [register_reference(Uri, POI) || POI <- POIs],
  ok;
index_references(_Document, 'shallow', _) ->
  ok.

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
  start(<<"OTP">>, entries_otp()),
  start(<<"Applications">>, entries_apps()),
  start(<<"Dependencies">>, entries_deps()).

-spec start(binary(), [{string(), 'deep' | 'shallow'}]) -> ok.
start(Group, Entries) ->
  SkipGeneratedFiles = els_config_indexing:get_skip_generated_files(),
  GeneratedFilesTag = els_config_indexing:get_generated_files_tag(),
  Task = fun({Dir, Mode}, _) ->
             index_dir(Dir, Mode, SkipGeneratedFiles, GeneratedFilesTag)
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

%%==============================================================================
%% Internal functions
%%==============================================================================


%% @doc Try indexing a file.
-spec try_index_file(binary(), mode(), boolean(), string()) ->
        ok | skipped | {error, any()}.
try_index_file(FullName, Mode, SkipGeneratedFiles, GeneratedFilesTag) ->
  Uri = els_uri:uri(FullName),
  try
    ?LOG_DEBUG("Indexing file. [filename=~s, uri=~s]", [FullName, Uri]),
    {ok, Text} = file:read_file(FullName),
    index_if_not_generated(Uri, Text, Mode,
                           SkipGeneratedFiles, GeneratedFilesTag)
  catch Type:Reason:St ->
      ?LOG_ERROR("Error indexing file "
                 "[filename=~s, uri=~s] "
                 "~p:~p:~p", [FullName, Uri, Type, Reason, St]),
      {error, {Type, Reason}}
  end.

-spec register_reference(uri(), poi()) -> ok.
register_reference(Uri, #{id := {F, A}} = POI) ->
  M = els_uri:module(Uri),
  register_reference(Uri, POI#{id => {M, F, A}});
register_reference(Uri, #{kind := Kind, id := Id, range := Range})
  when %% Include
       Kind =:= include;
       Kind =:= include_lib;
       %% Function
       Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= import_entry;
       %% Type
       Kind =:= type_application;
       %% Behaviour
       Kind =:= behaviour ->
  els_dt_references:insert( Kind
                          , #{id => Id, uri => Uri, range => Range}
                          ).

-spec index_dir(string(), mode()) ->
        {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Mode) ->
  SkipGeneratedFiles = els_config_indexing:get_skip_generated_files(),
  GeneratedFilesTag = els_config_indexing:get_generated_files_tag(),
  index_dir(Dir, Mode, SkipGeneratedFiles, GeneratedFilesTag).

-spec index_dir(string(), mode(), boolean(), string()) ->
        {non_neg_integer(), non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Mode, SkipGeneratedFiles, GeneratedFilesTag) ->
  ?LOG_DEBUG("Indexing directory. [dir=~s] [mode=~s]", [Dir, Mode]),
  F = fun(FileName, {Succeeded, Skipped, Failed}) ->
          case try_index_file(els_utils:to_binary(FileName), Mode,
                              SkipGeneratedFiles, GeneratedFilesTag) of
            ok -> {Succeeded + 1, Skipped, Failed};
            skipped -> {Succeeded, Skipped + 1, Failed};
            {error, _Error} -> {Succeeded, Skipped, Failed + 1}
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
  ?LOG_DEBUG("Finished indexing directory. [dir=~s] [mode=~s] [time=~p] "
             "[succeeded=~p] [skipped=~p] [failed=~p]",
             [Dir, Mode, Time/1000/1000, Succeeded, Skipped, Failed]),
  {Succeeded, Skipped, Failed}.

-spec entries_apps() -> [{string(), 'deep' | 'shallow'}].
entries_apps() ->
  [{Dir, 'deep'} || Dir <- els_config:get(apps_paths)].

-spec entries_deps() -> [{string(), 'deep' | 'shallow'}].
entries_deps() ->
  [{Dir, 'deep'} || Dir <- els_config:get(deps_paths)].

-spec entries_otp() -> [{string(), 'deep' | 'shallow'}].
entries_otp() ->
  [{Dir, 'shallow'} || Dir <- els_config:get(otp_paths)].
