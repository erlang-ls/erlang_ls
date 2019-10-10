-module(erlang_ls_index).

-behaviour(gen_server).

-callback index(binary(), any()) -> ok.
-callback setup() -> atom().

-export([ initialize/1
        , index_file/1
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

-spec index_file(file:name_all()) -> ok.
index_file(File) ->
  try
    {ok, Tree, Extra} = erlang_ls_parser:parse_file(File),
    AnnotatedTree = erlang_ls_tree:annotate(Tree, Extra),
    Uri = erlang_ls_uri:uri(File),
    [Index:index(Uri, AnnotatedTree) || Index <- ?INDEXES],
    ok
  catch Type:Reason:St ->
      lager:error("Error indexing ~s: ~p", [File, Reason]),
      erlang:raise(Type, Reason, St)
  end.

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
handle_cast(_Request, State) ->
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
