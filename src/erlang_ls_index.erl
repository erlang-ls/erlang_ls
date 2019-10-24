-module(erlang_ls_index).

-behaviour(gen_server).

-callback index(erlang_ls_document:document()) -> ok.
-callback setup() -> atom().

-export([ find_and_index_file/1
        , index/1
        , index_file/1
        , initialize/1
        , start_link/1
        ]).

-export([ app_path/0
        , deps_path/0
        , otp_path/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-type index() :: erlang_ls_completion_index
               | erlang_ls_references_index.
-type state() :: any().

-define( INDEXES
       , [ erlang_ls_completion_index
         , erlang_ls_references_index
         ]
       ).

-include("erlang_ls.hrl").

%%==============================================================================
%% External functions
%%==============================================================================

-spec initialize(map()) -> ok.
initialize(_Config) ->
  %% Initialize all indexes
  [  supervisor:start_child(erlang_ls_indexes_sup, [Index])
     || Index <- ?INDEXES
  ],
  %% TODO: This could be done asynchronously,
  %%       but we need a way to know when indexing is done,
  %%       or the tests will be flaky.

  %% At initialization, we currently index only the app path.
  %% deps and otp paths will be indexed on demand.
  indexer(app_path()),
  ok.

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Uri = erlang_ls_document:uri(Document),
  ok  = erlang_ls_db:store(documents, Uri, Document),
  [gen_server:cast(Index, {index, Index, Document}) || Index <- ?INDEXES],
  ok.

-spec start_link(index()) -> {ok, pid()}.
start_link(Index) ->
  gen_server:start_link({local, Index}, ?MODULE, Index, []).

%%==============================================================================
%% gen_server callbacks
%%==============================================================================

-spec init(index()) -> {ok, state()}.
init(Index) ->
  State = Index:setup(),
  {ok, State}.

-spec handle_call(any(), {pid(), any()}, state()) ->
  {reply, any(), state()}.
handle_call(_Message, _From, State) ->
  {reply, ok, State}.

-spec handle_cast(any(), any()) ->
  {noreply, state()}.
handle_cast({index, Index, Document}, State) ->
  Index:index(Document),
  {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec indexer([string()]) -> ok.
indexer(Paths) ->
  Fun = fun(File, _) -> index_file(iolist_to_binary(File)) end,
  [ filelib:fold_files(Path, ".*\\.[e,h]rl$", true, Fun, ok)
    || Path <- Paths
  ],
  ok.

-spec find_and_index_file(string()) ->
   {ok, uri()} | {error, any()}.
find_and_index_file(FileName) ->
  Paths = lists:append([ app_path()
                       , deps_path()
                       , otp_path()
                       ]),
  case file:path_open(Paths, list_to_binary(FileName), [read]) of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      index_file(FullName),
      {ok, erlang_ls_uri:uri(FullName)};
    {error, Error} ->
      {error, Error}
  end.

-spec index_file(file:name_all()) -> ok.
index_file(FullName) ->
  try
    lager:debug("Indexing ~s", [FullName]),
    {ok, Text} = file:read_file(FullName),
    Uri        = erlang_ls_uri:uri(FullName),
    Document   = erlang_ls_document:create(Uri, Text),
    ok         = index(Document)
  catch Type:Reason:St ->
      lager:error("Error indexing ~s: ~p", [FullName, Reason]),
      erlang:raise(Type, Reason, St)
  end.

-spec app_path() -> [string()].
app_path() ->
  {ok, RootUri} = erlang_ls_config:get(root_uri),
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  resolve_paths( [ [RootPath, "src"]
                 , [RootPath, "test"]
                 , [RootPath, "include"]
                 ]).

-spec deps_path() -> [string()].
deps_path() ->
  {ok, RootUri} = erlang_ls_config:get(root_uri),
  RootPath = binary_to_list(erlang_ls_uri:path(RootUri)),
  {ok, Dirs} = erlang_ls_config:get(deps_dirs),
  Paths = [ resolve_paths( [ [RootPath, Dir, "src"]
                           , [RootPath, Dir, "test"]
                           , [RootPath, Dir, "include"]
                           ])
            || Dir <- Dirs
          ],
  lists:append(Paths).

-spec otp_path() -> [string()].
otp_path() ->
  {ok, Root} = erlang_ls_config:get(otp_path),
  resolve_paths( [ [Root, "lib", "*", "src"]
                 , [Root, "lib", "*", "include"]
                 ]).

-spec resolve_paths([[string()]]) -> [[string()]].
resolve_paths(PathSpecs) ->
  lists:append([resolve_path(PathSpec) || PathSpec <- PathSpecs]).

-spec resolve_path([string()]) -> [string()].
resolve_path(PathSpec) ->
  Path = filename:join(PathSpec),
  Paths = [[P | subdirs(P)] || P <- filelib:wildcard(Path)],
  lists:append(Paths).

%% Returns all subdirectories for the provided path
-spec subdirs(string()) -> [string()].
subdirs(Path) ->
  subdirs(Path, []).

-spec subdirs(string(), [string()]) -> [string()].
subdirs(Path, Subdirs) ->
  case file:list_dir(Path) of
    {ok, Files}     -> subdirs_(Path, Files, Subdirs);
    {error, enoent} -> Subdirs
  end.

-spec subdirs_(string(), [string()], [string()]) -> [string()].
subdirs_(Path, Files, Subdirs) ->
  Fold = fun(F, Acc) ->
             FullPath = filename:join([Path, F]),
             case filelib:is_dir(FullPath) of
               true  -> subdirs(FullPath, [FullPath | Acc]);
               false -> Acc
             end
         end,
  lists:foldl(Fold, Subdirs, Files).
