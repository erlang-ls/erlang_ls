-module(erlang_ls_indexer).

-callback index(erlang_ls_document:document()) -> ok.

-export([ find_and_index_file/1
        , index/1
        , initialize/0
        , start_link/1
        ]).

-type index() :: erlang_ls_completion_index
               | erlang_ls_references_index
               | erlang_ls_specs_index.

-define( INDEXES
       , [ erlang_ls_completion_index
         , erlang_ls_references_index
         , erlang_ls_specs_index
         ]
       ).

-include("erlang_ls.hrl").

%%==============================================================================
%% External functions
%%==============================================================================

-spec initialize() -> ok.
initialize() ->
  %% TODO: This could be done asynchronously,
  %%       but we need a way to know when indexing is done,
  %%       or the tests will be flaky.

  %% At initialization, we currently index only the app path.
  %% deps and otp paths will be indexed on demand.
  [index_dir(Dir) || Dir <- erlang_ls_config:get(app_paths)],
  ok.

-spec index(erlang_ls_document:document()) -> ok.
index(Document) ->
  Uri    = erlang_ls_document:uri(Document),
  ok     = erlang_ls_db:store(documents, Uri, Document),
  [wpool:call(indexers, {Index, index, [Document]}) || Index <- ?INDEXES],
  ok.

-spec start_link(index()) -> {ok, pid()}.
start_link(Index) ->
  gen_server:start_link({local, Index}, ?MODULE, Index, []).

%%==============================================================================
%% Internal functions
%%==============================================================================

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
