-module(els_utils).

-export([ find_document/1
        , find_module/1
        , find_module/2
        , fold_files/4
        , project_relative/1
        , halt/1
        , start_epmd/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% File and module functions
%%==============================================================================

%% @equiv find_module(Module, erl)
-spec find_module(atom()) -> {ok, uri()} | {error, any()}.
find_module(M) ->
  find_module(M, erl).

%% @doc Look for a module in the DB.
%%
%% Look for a given module in the DB and return the respective
%% `URI'. If the module is not in the DB, try to index it.
-spec find_module(atom(), erl | hrl) -> {ok, uri()} | {error, any()}.
find_module(M, Extension) ->
  case els_dt_module:find_by_module(M) of
    {ok, [#{uri := Uri}]} ->
      {ok, Uri};
    {ok, []} ->
      FileName = atom_to_list(M) ++ extension(Extension),
      els_indexer:find_and_index_file(FileName, sync)
  end.

%% @doc Look for a document in the DB.
%%
%% Look for a given document in the DB and return it.
%% If the module is not in the DB, try to index it.
-spec find_document(uri()) ->
  {ok, els_document:document()} | {error, any()}.
find_document(Uri) ->
  case els_dt_document:lookup(Uri) of
    {ok, [#{document := Document}]} ->
      {ok, Document};
    {ok, []} ->
      %% TODO: Maybe we should not index this
      Path = els_uri:path(Uri),
      {ok, Uri} = els_indexer:index_file(Path, sync),
      {ok, _} = els_dt_document:lookup(Uri)
  end.

%% @doc Folds over all files in a directory recursively
%%
%% Applies function F to each file and the accumulator,
%% skipping all symlinks.
-spec fold_files(function(), function(), string(), any()) -> any().
fold_files(F, Filter, Dir, Acc) ->
  do_fold_dir(F, Filter, Dir, Acc).

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  erlang:halt(ExitCode).

-spec start_epmd() -> ok.
start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

%% @doc Returns a project-relative file path for a given URI
-spec project_relative(uri()) -> file:filename().
project_relative(Uri) ->
  RootUri     = els_config:get(root_uri),
  RootUriSize = byte_size(RootUri),
  <<RootUri:RootUriSize/binary, RelativePath/binary>> = Uri,
  binary_to_list(string:trim(RelativePath, leading, [$/, $\\ ])).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec epmd_path() -> string().
epmd_path() ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    Name = "epmd",
    case os:find_executable(Name, ErtsBinDir) of
        false ->
            case os:find_executable(Name) of
                false ->
                    error("Could not find epmd.");
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.

-spec extension(erl | hrl) -> string().
extension(erl) -> ".erl";
extension(hrl) -> "".

%% Folding over files

-spec do_fold_files(function(), function(), string(), [string()], any()) ->
  any().
do_fold_files(_F, _Filter,  _Dir, [], Acc0) ->
  Acc0;
do_fold_files(F, Filter, Dir, [File | Rest], Acc0) ->
  Path = filename:join(Dir, File),
  %% Symbolic links are not regular files
  Acc  = case filelib:is_regular(Path) of
           true  -> do_fold_file(F, Filter, Path, Acc0);
           false -> do_fold_dir(F, Filter, Path, Acc0)
         end,
  do_fold_files(F, Filter, Dir, Rest, Acc).

-spec do_fold_file(function(), function(), string(), any()) ->
  any().
do_fold_file(F, Filter, Path, Acc) ->
  case Filter(Path) of
    true  -> F(Path, Acc);
    false -> Acc
  end.

-spec do_fold_dir(function(), function(), string(), any()) ->
  any().
do_fold_dir(F, Filter, Dir, Acc) ->
  case not is_symlink(Dir) of
    true ->
      {ok, Files} = file:list_dir(Dir),
      do_fold_files(F, Filter, Dir, Files, Acc);
    false ->
      Acc
  end.

-spec is_symlink(string()) -> boolean().
is_symlink(Path) ->
  case file:read_link(Path) of
    {ok, _} -> true;
    {error, _} -> false
  end.
