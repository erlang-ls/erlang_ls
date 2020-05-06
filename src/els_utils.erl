-module(els_utils).

-export([ cmd/2
        , cmd/3
        , filename_to_atom/1
        , find_header/1
        , find_module/1
        , fold_files/4
        , halt/1
        , lookup_document/1
        , project_relative/1
        , resolve_paths/3
        , to_binary/1
        , to_list/1
        ]).

-include("erlang_ls.hrl").

-type path() :: file:filename_all().

%%==============================================================================
%% File and module functions
%%==============================================================================

-spec cmd(string(), [string()]) -> integer().
cmd(Cmd, Args) ->
  cmd(Cmd, Args, []).

% @doc Replacement for os:cmd that allows for spaces in args and paths
-spec cmd(string(), [string()], string()) -> integer().
cmd(Cmd, Args, Path) ->
  lager:info("Running OS command [command=~p] [args=~p]", [Cmd, Args]),
  Executable = case filename:basename(Cmd) of
                 Cmd ->
                   cmd_path(Cmd);
                 _ ->
                   %% The command already contains a path
                   Cmd
               end,
  Tag = make_ref(),
  F =
    fun() ->
        P = open_port(
              {spawn_executable, Executable},
              [ binary
              , use_stdio
              , stream
              , exit_status
              , hide
              , {args, Args}
                %% TODO: Windows-friendly version?
              , {env, [{"PATH", Path ++ ":" ++ os:getenv("PATH")}]}
              ]
             ),
        exit({Tag, cmd_receive(P)})
    end,
  {Pid, Ref} = erlang:spawn_monitor(F),
  receive
    {'DOWN', Ref, process, Pid, {Tag, Data}} ->
      Data;
    {'DOWN', Ref, process, Pid, Reason} ->
      exit(Reason)
  end.

%% @doc Return the path for a command
-spec cmd_path(string()) -> string().
cmd_path(Cmd) ->
  ErtsBinDir = filename:dirname(escript:script_name()),
  case os:find_executable(Cmd, ErtsBinDir) of
    false ->
      case os:find_executable(Cmd) of
        false ->
          Fmt = "Could not find command ~p",
          Args = [Cmd],
          Msg = lists:flatten(io_lib:format(Fmt, Args)),
          error(Msg);
        GlobalEpmd ->
          GlobalEpmd
      end;
    Epmd ->
      Epmd
  end.

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
      els_indexing:find_and_index_file(FileName)
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
      els_indexing:find_and_index_file(FileName)
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
      {ok, Uri} = els_indexing:index_file(Path),
      {ok, [Document]} = els_dt_document:lookup(Uri),
      {ok, Document}
  end.

%% @doc Folds over all files in a directory recursively
%%
%% Applies function F to each file and the accumulator,
%% skipping all symlinks.
-spec fold_files(function(), function(), path(), any()) -> any().
fold_files(F, Filter, Dir, Acc) ->
  do_fold_dir(F, Filter, Dir, Acc).

%% @doc Resolve paths based on path specs
%%
%% Gets a list of path specs and returns the expanded list of paths.
%% Path specs can contains glob expressions. Resolved paths that contain
%% symlinks will be ignored.
-spec resolve_paths([[path()]], path(), boolean()) -> [[path()]].
resolve_paths(PathSpecs, RootPath, Recursive) ->
  lists:append([ resolve_path(PathSpec, RootPath, Recursive)
                 || PathSpec <- PathSpecs
               ]).

-spec halt(non_neg_integer()) -> ok.
halt(ExitCode) ->
  els_db:stop(),
  ok = init:stop(ExitCode).

%% @doc Returns a project-relative file path for a given URI
-spec project_relative(uri()) -> file:filename() | {error, not_relative}.
project_relative(Uri) ->
  RootUri = els_config:get(root_uri),
  Size    = byte_size(RootUri),
  case Uri of
    <<RootUri:Size/binary, Relative/binary>> ->
      Trimmed = string:trim(Relative, leading, [$/, $\\ ]),
      to_list(Trimmed);
    _ ->
      {error, not_relative}
  end.

-spec to_binary(unicode:chardata()) -> binary().
to_binary(X) when is_binary(X) ->
  X;
to_binary(X) when is_list(X) ->
  case unicode:characters_to_binary(X) of
    Result when is_binary(Result) -> Result;
    _ -> iolist_to_binary(X)
  end.

-spec to_list(unicode:chardata()) -> string().
to_list(X) when is_list(X) ->
  X;
to_list(X) when is_binary(X) ->
  case unicode:characters_to_list(X) of
    Result when is_list(Result) -> Result;
    _ -> binary_to_list(X)
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

%% Folding over files

-spec do_fold_files(function(), function(), path(), [path()], any()) -> any().
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

-spec do_fold_file(function(), function(), path(), any()) ->
  any().
do_fold_file(F, Filter, Path, Acc) ->
  case Filter(Path) of
    true  -> F(Path, Acc);
    false -> Acc
  end.

-spec do_fold_dir(function(), function(), path(), any()) ->
  any().
do_fold_dir(F, Filter, Dir, Acc) ->
  case not is_symlink(Dir) andalso filelib:is_dir(Dir) of
    true ->
      {ok, Files} = file:list_dir(Dir),
      do_fold_files(F, Filter, Dir, Files, Acc);
    false ->
      Acc
  end.

-spec is_symlink(path()) -> boolean().
is_symlink(Path) ->
  case file:read_link(Path) of
    {ok, _} -> true;
    {error, _} -> false
  end.

%% @doc Resolve paths recursively

-spec resolve_path([path()], path(), boolean()) -> [path()].
resolve_path(PathSpec, RootPath, Recursive) ->
  RPath = filename:join(PathSpec),
  Paths = filelib:wildcard(RPath),
  RecAdd = fun(Path) ->
               SubDirs = [P || P <- subdirs(Paths),
                               contains_src_files(P)],
               case contains_src_files(Path) of
                 true  -> [make_normalized_path(Path) | SubDirs];
                 false -> SubDirs
               end
             end,
  case Recursive of
    true  ->
      lists:append([ RecAdd(P) || P <- Paths,
                                  not contains_symlink(P, RootPath)]);
    false ->
      [make_normalized_path(P) || P <- Paths,
                                  not contains_symlink(P, RootPath),
                                  contains_src_files(P)]
  end.

%% Returns all subdirectories for the provided path
-spec subdirs(path()) -> [path()].
subdirs(Path) ->
  subdirs(Path, []).

-spec subdirs(path(), [path()]) -> [path()].
subdirs(Path, Subdirs) ->
  case file:list_dir(Path) of
    {ok, Files} -> subdirs_(Path, Files, Subdirs);
    {error, _}  -> Subdirs
  end.

-spec subdirs_(path(), [path()], [path()]) -> [path()].
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

-spec contains_src_files(path()) -> boolean().
contains_src_files(P) ->
  [] /= filelib:wildcard(filename:join(P, "*.?rl")).

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
      ((not (Parent == RootPath)) and (is_symlink(Parent)))
        orelse contains_symlink(Parent, RootPath)
  end.

-spec cmd_receive(port()) -> integer().
cmd_receive(Port) ->
  receive
    {Port, {exit_status, ExitCode}} ->
      ExitCode;
    {Port, _} ->
      cmd_receive(Port)
  end.

%%==============================================================================
%% This section excerpted from the rebar3 sources, rebar_dir.erl
%% Pending resolution of https://github.com/erlang/rebar3/issues/2223
%%==============================================================================

%% @doc make a path absolute
-spec make_absolute_path(path()) -> path().
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
-spec make_normalized_path(path()) -> path().
make_normalized_path(Path) ->
  AbsPath = make_absolute_path(Path),
  Components = filename:split(AbsPath),
  make_normalized_path(Components, []).

%% @private drops path fragments for normalization
-spec make_normalized_path([file:name_all()], [file:name_all()]) -> path().
make_normalized_path([], NormalizedPath) ->
  filename:join(lists:reverse(NormalizedPath));
make_normalized_path(["." | []], []) ->
  ".";
make_normalized_path(["." | T], NormalizedPath) ->
  make_normalized_path(T, NormalizedPath);
make_normalized_path([".." | T], []) ->
  make_normalized_path(T, [".."]);
make_normalized_path([".." | T], [Head | Tail]) when Head =/= ".." ->
  make_normalized_path(T, Tail);
make_normalized_path([H | T], NormalizedPath) ->
  make_normalized_path(T, [H | NormalizedPath]).
