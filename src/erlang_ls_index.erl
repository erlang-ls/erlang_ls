-module(erlang_ls_index).

-behaviour(gen_server).

-callback index(erlang_ls_document:document()) -> ok.
-callback setup() -> atom().

-export([ index/1
        , initialize/1
        , start_link/1
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

-type index() :: erlang_ls_references_index.
-type state() :: any().

-define( INDEXES
       , [erlang_ls_references_index]
       ).

%%==============================================================================
%% External functions
%%==============================================================================

-spec initialize(map()) -> ok.
initialize(_Config) ->
  %% Initialize all indexes
  [  supervisor:start_child(erlang_ls_indexes_sup, [Index])
     || Index <- ?INDEXES
  ],
  %% Start indexing process
  erlang:spawn(fun indexer/0),
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

-spec indexer() -> ok.
indexer() ->
  Paths = erlang_ls_code_navigation:app_path(),
  Fun   = fun(File, _) -> index_file(iolist_to_binary(File)) end,
  [ filelib:fold_files(Path, ".*\\.erl$", true, Fun, ok)
    || Path <- Paths
  ],
  ok.

-spec index_file(file:name_all()) -> ok.
index_file(File) ->
  try
    lager:debug("Indexing ~s", [File]),
    {ok, Text} = file:read_file(File),
    Uri        = erlang_ls_uri:uri(File),
    Document   = erlang_ls_document:create(Uri, Text),
    ok         = index(Document)
  catch Type:Reason:St ->
      lager:error("Error indexing ~s: ~p", [File, Reason]),
      erlang:raise(Type, Reason, St)
  end.
