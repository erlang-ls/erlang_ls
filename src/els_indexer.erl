%% TODO: Show progress messages to client
-module(els_indexer).

%% TODO: Move all code in one place
%% TODO: Rename tables
-callback index(els_document:document()) -> ok.

%% TODO: Solve API mix (gen_server and not)
%% API
-export([ find_and_index_file/1
        , find_and_index_file/2
        , index_file/2
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
-include_lib("stdlib/include/ms_transform.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: #{}.

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
  find_and_index_file(FileName, async).

-spec find_and_index_file(string(), async | sync) ->
   {ok, uri()} | {error, any()}.
find_and_index_file(FileName, SyncAsync) ->
  SearchPaths = els_config:get(search_paths),
  case file:path_open(SearchPaths, list_to_binary(FileName), [read]) of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      index_file(FullName, SyncAsync);
    {error, Error} ->
      {error, Error}
  end.

-spec index_file(binary(), sync | async) -> {ok, uri()}.
index_file(Path, SyncAsync) ->
  try_index_file(Path, SyncAsync),
  {ok, els_uri:uri(Path)}.

-spec index(els_document:document()) -> ok.
index(Document) ->
  Uri    = els_document:uri(Document),
  ok     = els_dt_documents:insert(#{uri => Uri, document => Document}),
  Module = els_uri:module(Uri),
  els_dt_module:insert(#{module => Module, uri => Uri}),
  Specs  = els_document:points_of_interest(Document, [spec]),
  [els_dt_signatures:insert(#{ mfa  => {Module, F, A}
                             , tree => Tree
                             }) ||
    #{id := {F, A}, data := Tree} <- Specs],
  Kinds = [application, implicit_fun],
  POIs  = els_document:points_of_interest(Document, Kinds),
  %% TODO: Transaction
  ok = els_dt_references:delete_by_uri(Uri),
  [register_reference(Uri, POI) || POI <- POIs],
  ok.

-spec index_app() -> any().
index_app() ->
  gen_server:cast(?SERVER, {index_app}).

-spec index_deps() -> any().
index_deps() ->
  case application:get_env(erlang_ls, index_deps) of
    {ok, true}  -> gen_server:cast(?SERVER, {index_deps});
    {ok, false} -> lager:info("Not indexing dependencies")
  end.

-spec index_otp() -> any().
index_otp() ->
  case application:get_env(erlang_ls, index_otp) of
    {ok, true}  -> gen_server:cast(?SERVER, {index_otp});
    {ok, false} -> lager:info("Not indexing OTP")
  end.

-spec index_dir(string()) -> {non_neg_integer(), non_neg_integer()}.
index_dir(Dir) ->
  lager:info("Indexing directory. [dir=~s]", [Dir]),
  F = fun(FileName, {Succeeded, Failed}) ->
          case try_index_file(list_to_binary(FileName), async) of
            ok              -> {Succeeded + 1, Failed};
            {error, _Error} -> {Succeeded, Failed + 1}
          end
      end,
  Filter = fun(Path) ->
               Ext = filename:extension(Path),
               lists:member(Ext, [".erl", ".hrl"])
           end,

  {Time, {Succeeded, Failed}} = timer:tc( els_utils
                                        , fold_files
                                        , [ F
                                          , Filter
                                          , Dir
                                          , {0, 0}
                                          ]
                                        ),
  lager:info("Finished indexing directory. [dir=~s] [time=~p] "
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
  Workers = application:get_env(els_app, indexers, 10),
  {ok, _Pool} = wpool:start_sup_pool(indexers, [ {workers, Workers} ]),
  {ok, #{}}.

-spec handle_call(any(), any(), state()) ->
  {noreply, state()}.
handle_call(_Request, _From, State) ->
  {noreply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({index_app}, State) ->
  [index_dir(Dir) || Dir <- els_config:get(app_paths)],
  {noreply, State};
handle_cast({index_deps}, State) ->
  [index_dir(Dir) || Dir <- els_config:get(deps_paths)],
  {noreply, State};
handle_cast({index_otp}, State) ->
  [index_dir(Dir) || Dir <- els_config:get(otp_paths)],
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
-spec try_index_file(binary(), sync | async) -> ok | {error, any()}.
try_index_file(FullName, SyncAsync) ->
  try
    lager:debug("Indexing file. [filename=~s]", [FullName]),
    {ok, Text} = file:read_file(FullName),
    Uri        = els_uri:uri(FullName),
    Document   = els_document:create(Uri, Text),
    ok         = index_document(Document, SyncAsync)
  catch Type:Reason:St ->
      lager:error("Error indexing file "
                  "[filename=~s] "
                  "~p:~p:~p", [FullName, Type, Reason, St]),
      {error, {Type, Reason}}
  end.

-spec index_document(els_document:document(), async | sync) -> ok.
index_document(Document, async) ->
  ok = wpool:cast(indexers, {?MODULE, index, [Document]});
index_document(Document, sync) ->
  %% Don't use the pool for synchronous indexing
  ok = index(Document).

-spec register_reference(uri(), poi()) -> ok.
register_reference(Uri, #{id := {F, A}} = POI) ->
  M = els_uri:module(Uri),
  register_reference(Uri, POI#{ id => {M, F, A}});
register_reference(Uri, #{id := {M, F, A}, range := Range}) ->
  els_dt_references:insert(#{ id    => {M, F, A}
                            , uri   => Uri
                            , range => Range
                            }).
