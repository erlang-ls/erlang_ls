-module(els_utils).

-export([ filename_to_atom/1
        , find_header/1
        , find_module/1
        , fold_files/4
        , lookup_document/1
        , project_relative/1
        , resolve_paths/3
        , halt/1
        ]).

-include("erlang_ls.hrl").

-type path() :: file:filename().

%%==============================================================================
%% File and module functions
%%==============================================================================

-spec filename_to_atom(els_dt_document:id()) -> atom().
filename_to_atom(FileName) ->
  list_to_atom(filename:basename(FileName, filename:extension(FileName))).

%% @doc Look for a header in the DB
-spec find_header(atom()) -> {ok, uri()} | {error, any()}.
find_header(Id) ->
  {ok, Candidates} = els_dt_document_index:lookup(Id),
  case [Uri || #{kind := header, uri := Uri} <- Candidates] of
    [Uri | _] ->
      {ok, Uri};
    [] ->
      FileName = atom_to_list(Id) ++ ".hrl",
      els_indexer:find_and_index_file(FileName)
  end.

%% @doc Look for a module in the DB
-spec find_module(atom()) -> {ok, uri()} | {error, any()}.
find_module(Id) ->
  {ok, Candidates} = els_dt_document_index:lookup(Id),
  case [Uri || #{kind := module, uri := Uri} <- Candidates] of
    [Uri | _] ->
      {ok, Uri};
    [] ->
      FileName = atom_to_list(Id) ++ ".erl",
      els_indexer:find_and_index_file(FileName)
  end.

%% @doc Look for a document in the DB.
%%
%% Look for a given document in the DB and return it.
%% If the module is not in the DB, try to index it.
-spec lookup_document(uri()) ->
  {ok, els_dt_document:item()} | {error, any()}.
lookup_document(Uri) ->
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      {ok, Document};
    {ok, []} ->
      Path = els_uri:path(Uri),
      {ok, Uri} = els_indexer:index_file(Path),
      {ok, [Document]} = els_dt_document:lookup(Uri),
      {ok, Document}
  end.

%% @doc Folds over all files in a directory recursively
%%
%% Applies function F to each file and the accumulator,
%% skipping all symlinks.
-spec fold_files(function(), function(), string(), any()) -> any().
fold_files(F, Filter, Dir, Acc) ->
  do_fold_dir(F, Filter, Dir, Acc).

%% @doc Resolve paths based on path specs
%%
%% Gets a list of path specs and returns the expanded list of paths.
%% Path specs can contains glob expressions. Resolved paths that contain
%% symlinks will be ignored.
-spec resolve_paths([[string()]], path(), boolean()) -> [[string()]].
resolve_paths(PathSpecs, RootPath, Recursive) ->
  lists:append([ resolve_path(PathSpec, RootPath, Recursive)
                 || PathSpec <- PathSpecs
               ]).

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  els_db:stop(),
  ok = init:stop(ExitCode).

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
  case not is_symlink(Dir) andalso filelib:is_dir(Dir) of
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

%% @doc Resolve paths recursively

-spec resolve_path([string()], path(), boolean()) -> [string()].
resolve_path(PathSpec, RootPath, Recursive) ->
  Path  = filename:join(PathSpec),
  Paths = filelib:wildcard(Path),

  case Recursive of
    true  ->
      lists:append([ [make_normalized_path(P) | subdirs(P)]
                     || P <- Paths, not contains_symlink(P, RootPath)
                   ]);
    false ->
      [make_normalized_path(P) || P <- Paths, not contains_symlink(P, RootPath)]
  end.

%% Returns all subdirectories for the provided path
-spec subdirs(string()) -> [string()].
subdirs(Path) ->
  subdirs(Path, []).

-spec subdirs(string(), [string()]) -> [string()].
subdirs(Path, Subdirs) ->
  case file:list_dir(Path) of
    {ok, Files} -> subdirs_(Path, Files, Subdirs);
    {error, _}  -> Subdirs
  end.

-spec subdirs_(string(), [string()], [string()]) -> [string()].
subdirs_(Path, Files, Subdirs) ->
  Fold = fun(F, Acc) ->
             FullPath = filename:join([Path, F]),
             case
               not is_symlink(FullPath)
               andalso filelib:is_dir(FullPath)
             of
               true  -> subdirs(FullPath, [FullPath | Acc]);
               false -> Acc
             end
         end,
  lists:foldl(Fold, Subdirs, Files).

-spec contains_symlink(path(), path()) -> boolean().
contains_symlink(RootPath, RootPath) ->
  false;
contains_symlink([], _RootPath) ->
  false;
contains_symlink(Path, RootPath) ->
  Parts = filename:split(Path),
  case lists:droplast(Parts) of
    [] -> false;
    ParentParts ->
      Parent = filename:join(ParentParts),
      is_symlink(Parent) orelse contains_symlink(Parent, RootPath)
  end.

%%==============================================================================
%% This section excerpted from the rebar3 sources, rebar_dir.erl
%% Pending resolution of https://github.com/erlang/rebar3/issues/2223
%%==============================================================================

%% @doc make a path absolute
-spec make_absolute_path(file:filename()) -> file:filename().
make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

%% @doc normalizing a path removes all of the `..' and the
%% `.' segments it may contain.
-spec make_normalized_path(file:filename()) -> file:filename().
make_normalized_path(Path) ->
    AbsPath = make_absolute_path(Path),
    Components = filename:split(AbsPath),
    make_normalized_path(Components, []).

%% @private drops path fragments for normalization
-spec make_normalized_path([string()], [string()]) -> file:filename().
make_normalized_path([], NormalizedPath) ->
    filename:join(lists:reverse(NormalizedPath));
make_normalized_path([H|T], NormalizedPath) ->
    case H of
        "." when NormalizedPath == [], T == []
                 -> make_normalized_path(T, ["."]);
        "."  -> make_normalized_path(T, NormalizedPath);
        ".." when NormalizedPath == [] -> make_normalized_path(T, [".."]);
        ".." when hd(NormalizedPath) =/= ".."
                  -> make_normalized_path(T, tl(NormalizedPath));
        _    -> make_normalized_path(T, [H|NormalizedPath])
    end.
