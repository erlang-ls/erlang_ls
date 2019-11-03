%% TODO: Show progress messages to client
-module(erlang_ls_indexer).

%% TODO: Move all code in one place
%% TODO: Rename tables
-callback index(erlang_ls_document:document()) -> ok.

%% TODO: Solve API mix (gen_server and not)
%% API
-export([ find_and_index_file/1
        , index/1
        , index_dir/1
        , start_link/0
        , index_app/0
        , index_deps/0
        , index_otp/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , terminate/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: #{}.

%%==============================================================================
%% Macros
%%==============================================================================
-define(SERVER, ?MODULE).
-define(INDEXES, [ erlang_ls_references_index
                 , erlang_ls_specs_index
                 ]
       ).

%%==============================================================================
%% Exported functions
%%==============================================================================
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

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Uri    = erlang_ls_document:uri(Document),
  ok     = erlang_ls_db:store(documents, Uri, Document),
  Module = erlang_ls_uri:module(Uri),
  ok = erlang_ls_db:store(completion_index, Module, Uri),
  [Index:index(Document) || Index <- ?INDEXES],
  ok.

-spec index_app() -> any().
index_app() ->
  gen_server:cast(?SERVER, {index_app}).

-spec index_deps() -> any().
index_deps() ->
  gen_server:cast(?SERVER, {index_deps}).

-spec index_otp() -> any().
index_otp() ->
  gen_server:cast(?SERVER, {index_otp}).

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
             "[failed=~p]", [Dir, Time/1000/1000, Succeeded, Failed]),
  {Succeeded, Failed}.


-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================

-spec init({}) -> {ok, state()}.
init({}) ->
  %% TODO: Optionally configure number of workers from args
  Workers = application:get_env(erlang_ls_app, indexers, 10),
  {ok, _Pool} = wpool:start_sup_pool(indexers, [ {workers, Workers} ]),
  {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
  {noreply, state()}.
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({index_app}, State) ->
  [index_dir(Dir) || Dir <- erlang_ls_config:get(app_paths)],
  {noreply, State};
handle_cast({index_deps}, State) ->
  [index_dir(Dir) || Dir <- erlang_ls_config:get(deps_paths)],
  {noreply, State};
handle_cast({index_otp}, State) ->
  [index_dir(Dir) || Dir <- erlang_ls_config:get(otp_paths)],
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_, _State) ->
  wpool:stop_sup_pool(indexers),
  ok.

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
    ok         = wpool:cast(indexers, {?MODULE, index, [Document]})
  catch Type:Reason:St ->
      lager:error("Error indexing file "
                  "[filename=~s] "
                  "~p:~p:~p", [FullName, Type, Reason, St]),
      {error, {Type, Reason}}
  end.
