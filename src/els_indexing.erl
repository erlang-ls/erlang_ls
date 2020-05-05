-module(els_indexing).

-callback index(els_dt_document:item()) -> ok.

%% API
-export([ find_and_index_file/1
        , index_file/1
        , index/3
        , index_dir/2
        , start/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("erlang_bs/include/erlang_bs.hrl").

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
    _ ->
      Document = els_dt_document:new(Uri, Text),
      F = fun() ->
              do_index(Document, Mode)
          end,
      els_db:transaction(F)
  end.

-spec do_index(els_dt_document:item(), mode()) -> ok.
do_index(#{uri := Uri, id := Id, kind := Kind} = Document, Mode) ->
  ok = els_dt_document:insert(Document),
  %% Mapping from document id to uri
  ModuleItem = els_dt_document_index:new(Id, Uri, Kind),
  ok = els_dt_document_index:insert(ModuleItem),
  index_signatures(Document),
  index_references(Document, Mode).

-spec index_signatures(els_dt_document:item()) -> ok.
index_signatures(#{id := Id} = Document) ->
  Specs  = els_dt_document:pois(Document, [spec]),
  [ els_dt_signatures:insert(#{ mfa => {Id, F, A}, spec => Spec})
    || #{id := {F, A}, data := Spec} <- Specs
  ],
  ok.

-spec index_references(els_dt_document:item(), mode()) -> ok.
index_references(#{uri := Uri} = Document, 'deep') ->
  %% References
  POIs  = els_dt_document:pois(Document, [ application
                                         , behaviour
                                         , implicit_fun
                                         , macro
                                         , record_access
                                         , record_expr
                                         , type_application
                                         ]),
  ok = els_dt_references:delete_by_uri(Uri),
  [register_reference(Uri, POI) || POI <- POIs],
  ok;
index_references(_Document, 'shallow') ->
  ok.

-spec start() -> ok.
start() ->
  #{targets := Targets} = els_build_server:request( <<"workspace/targets">>
                                                  , #{}),
  start(<<"OTP">>, entries_otp()),
  start(<<"Applications">>, entries_apps(Targets)),
  start(<<"Dependencies">>, entries_deps()).

-spec start(binary(), [{string(), 'deep' | 'shallow'}]) -> ok.
start(Group, Entries) ->
  Task = fun({Dir, Mode}, _) -> index_dir(Dir, Mode) end,
  Config = #{ task => Task
            , entries => Entries
              %% Indexing a directory can lead to a huge number
              %% of DB transactions happening in a very short
              %% time window. After indexing, let's manually
              %% trigger a DB dump. This ensures that the DB can
              %% be loaded much faster on a restart.
            , on_complete => fun(_) -> els_db:dump_tables() end
            , on_error => fun(_) -> els_db:dump_tables() end
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
  try
    Uri = els_uri:uri(FullName),
    lager:debug("Indexing file. [filename=~s]", [FullName]),
    {ok, Text} = file:read_file(FullName),
    ok         = index(Uri, Text, Mode)
  catch Type:Reason:St ->
      lager:error("Error indexing file "
                  "[filename=~s] "
                  "~p:~p:~p", [FullName, Type, Reason, St]),
      {error, {Type, Reason}}
  end.

-spec register_reference(uri(), poi()) -> ok.
register_reference(Uri, #{id := {F, A}} = POI) ->
  M = els_uri:module(Uri),
  register_reference(Uri, POI#{id => {M, F, A}});
register_reference(Uri, #{kind := Kind, id := Id, range := Range})
  when %% Record
       Kind =:= record_expr;
       Kind =:= record_access;
       %% Macro
       Kind =:= macro;
       %% Function
       Kind =:= application;
       Kind =:= implicit_fun;
       %% Type
       Kind =:= type_application;
       %% Behaviour
       Kind =:= behaviour ->
  els_dt_references:insert( Kind
                          , #{id => Id, uri => Uri, range => Range}
                          ).

-spec index_dir(string(), mode()) -> {non_neg_integer(), non_neg_integer()}.
index_dir(Dir, Mode) ->
  lager:debug("Indexing directory. [dir=~s] [mode=~s]", [Dir, Mode]),
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
  lager:debug("Finished indexing directory. [dir=~s] [mode=~s] [time=~p] "
             "[succeeded=~p] "
             "[failed=~p]", [Dir, Mode, Time/1000/1000, Succeeded, Failed]),
  {Succeeded, Failed}.

-spec entries_apps([els_build_server:target_id()]) ->
        [{string(), 'deep' | 'shallow'}].
entries_apps(Targets) ->
  {ok, #{items := Items}} = els_build_server:request( <<"buildTarget/sources">>
                                                    , #{targets => Targets}
                                                    ),
  Sources = lists:flatten([S || #{sources := S} <- Items]),
  [{Uri, 'deep'} || #{uri := Uri, kind := ?SOURCE_ITEM_KIND_DIR} <- Sources].

-spec entries_deps() -> [{string(), 'deep' | 'shallow'}].
entries_deps() ->
  [{Dir, 'deep'} || Dir <- els_config:get(deps_paths)].

-spec entries_otp() -> [{string(), 'deep' | 'shallow'}].
entries_otp() ->
  [{Dir, 'shallow'} || Dir <- els_config:get(otp_paths)].
