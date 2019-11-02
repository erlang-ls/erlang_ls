%% TODO: Show progress messages to client
-module(erlang_ls_indexer).

-callback index(erlang_ls_document:document()) -> ok.

%% TODO: Solve API mix (gen_server and not)
%% API
-export([ do_index/1
        , find_and_index_file/1
        , index/1
        , index_dir/1
        , start_link/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: #{}.

-define( INDEXES
       , [ erlang_ls_completion_index
         , erlang_ls_references_index
         , erlang_ls_specs_index
         ]
       ).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Exported functions
%%==============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  %% TODO: Optionally configure number of workers from args
  Workers = application:get_env(erlang_ls_app, indexers, 10),
  {ok, _Pool} = wpool:start_sup_pool(indexers, [ {workers, Workers} ]),
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  gen_server:call(?SERVER, {index, Document}).

%% TODO: Not necessary after indexing
-spec find_and_index_file(string()) ->
   {ok, uri()} | {error, any()}.
find_and_index_file(FileName) ->
  Paths = lists:append([ erlang_ls_config:get(app_paths)
                       , erlang_ls_config:get(deps_paths)
                       , erlang_ls_config:get(otp_paths)
                       ]),
  case file:path_open(Paths, list_to_binary(FileName), [read]) of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      try_index_file(FullName),
      {ok, erlang_ls_uri:uri(FullName)};
    {error, Error} ->
      {error, Error}
  end.

-spec do_index(erlang_ls_document:document()) -> ok.
do_index(Document) ->
  Uri    = erlang_ls_document:uri(Document),
  ok     = erlang_ls_db:store(documents, Uri, Document),
  [Index:index(Document) || Index <- ?INDEXES],
  ok.

%% @edoc Index a directory.
%%
%% Index all .erl and .hrl files contained in the given directory, recursively.
%% If indexing fails for a specific file, the file is skipped.
%% Return the number of correctly and incorrectly indexed files.
%%
-spec index_dir(string()) -> {non_neg_integer(), non_neg_integer()}.
index_dir(Dir) ->
  lager:info("Indexing directory. [dir=~s]", [Dir]),
  F = fun(FileName, {Succeeded, Failed}) ->
          case try_index_file(list_to_binary(FileName)) of
            ok              -> {Succeeded +1, Failed};
            {error, _Error} -> {Succeeded, Failed + 1}
          end
      end,
  {Time, {Succeeded, Failed}} = timer:tc( filelib
                                        , fold_files
                                        , [ Dir
                                          , ".*\\.[e,h]rl$"
                                          , true
                                          , F
                                          , {0, 0} ]),
  lager:info("Finished indexing directory. [dir=~s] [time=~p]"
             "[succeeded=~p] "
             "[failed=~p]", [Time/1000/1000, Dir, Succeeded, Failed]),
  {Succeeded, Failed}.

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
  {reply, any(), state()}.
handle_call({index, Document}, _From, State) ->
  {ok, Res} = wpool:call(indexers, {?MODULE, do_index, [Document]}),
  {reply, Res, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% @edoc Try indexing a file.
-spec try_index_file(binary()) -> ok | {error, any()}.
try_index_file(FullName) ->
  try
    lager:debug("Indexing file. [filename=~s]", [FullName]),
    {ok, Text} = file:read_file(FullName),
    Uri        = erlang_ls_uri:uri(FullName),
    Document   = erlang_ls_document:create(Uri, Text),
    ok         = index(Document)
  catch Type:Reason:St ->
      lager:error("Error indexing file "
                  "[filename=~s] "
                  "~p:~p:~p", [FullName, Type, Reason, St]),
      {error, {Type, Reason}}
  end.
