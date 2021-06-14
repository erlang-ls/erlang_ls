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
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

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
  try_index_file(Path, 'deep'),
  {ok, els_uri:uri(Path)}.

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
  case els_config:get(indexing_enabled) =:= false of
    false ->
      start(),
      true;
    true ->
      ?LOG_INFO("Skipping Indexing (disabled via InitOptions)"),
      false
  end.

-spec start() -> ok.
start() ->
  start(<<"OTP">>, entries_otp()),
  start(<<"Applications">>, entries_apps()),
  start(<<"Dependencies">>, entries_deps()).

-spec start(binary(), [{string(), 'deep' | 'shallow'}]) -> ok.
start(Group, Entries) ->
  Task = fun({Dir, Mode}, _) -> index_dir(Dir, Mode) end,
  Config = #{ task => Task
            , entries => Entries
            , title => <<"Indexing ", Group/binary>>
            },
  {ok, _Pid} = els_background_job:new(Config),
  ok.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% @doc Try indexing a file.
-spec try_index_file(binary(), mode()) -> ok | {error, any()}.
try_index_file(FullName, Mode) ->
  Uri = els_uri:uri(FullName),
  try
    ?LOG_DEBUG("Indexing file. [filename=~s, uri=~s]", [FullName, Uri]),
    {ok, Text} = file:read_file(FullName),
    ok         = index(Uri, Text, Mode)
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

-spec index_dir(string(), mode()) -> {non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Mode) ->
  ?LOG_DEBUG("Indexing directory. [dir=~s] [mode=~s]", [Dir, Mode]),
  F = fun(FileName, {Succeeded, Failed}) ->
          case try_index_file(els_utils:to_binary(FileName), Mode) of
            ok              -> {Succeeded + 1, Failed};
            {error, _Error} -> {Succeeded, Failed + 1}
          end
      end,
  Filter = fun(Path) ->
               Ext = filename:extension(Path),
               lists:member(Ext, [".erl", ".hrl", ".escript"])
           end,

  {Time, {Succeeded, Failed}} = timer:tc( els_utils
                                        , fold_files
                                        , [ F
                                          , Filter
                                          , Dir
                                          , {0, 0}
                                          ]
                                        ),
  ?LOG_DEBUG("Finished indexing directory. [dir=~s] [mode=~s] [time=~p] "
             "[succeeded=~p] "
             "[failed=~p]", [Dir, Mode, Time/1000/1000, Succeeded, Failed]),
  {Succeeded, Failed}.

-spec entries_apps() -> [{string(), 'deep' | 'shallow'}].
entries_apps() ->
  [{Dir, 'deep'} || Dir <- els_config:get(apps_paths)].

-spec entries_deps() -> [{string(), 'deep' | 'shallow'}].
entries_deps() ->
  [{Dir, 'deep'} || Dir <- els_config:get(deps_paths)].

-spec entries_otp() -> [{string(), 'deep' | 'shallow'}].
entries_otp() ->
  [{Dir, 'shallow'} || Dir <- els_config:get(otp_paths)].
