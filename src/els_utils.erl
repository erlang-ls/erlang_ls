-module(els_utils).

-export([ find_document/1
        , find_module/1
        , find_module/2
        , fold_files/4
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
  case els_db:find(modules, M) of
    {ok, Uri} ->
      {ok, Uri};
    {error, not_found} ->
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
  case els_db:find(documents, Uri) of
    {ok, Document} ->
      {ok, Document};
    {error, not_found} ->
      Path = els_uri:path(Uri),
      {ok, Uri} = els_indexer:index_file(Path, sync),
      {ok, _} = els_db:find(documents, Uri)
  end.

%% @doc Folds over all files in a directory recursively
%%
%% Applies function F to each file and the accumulator,
%% skipping all symlinks.
-spec fold_files(function(), function(), string(), any()) -> any().
fold_files(F, Filter, Dir, Acc) ->
  {ok, AllFiles} = file:list_dir(Dir),
  do_fold_files(F, Filter, Dir, AllFiles, Acc).

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  erlang:halt(ExitCode).

-spec start_epmd() -> ok.
start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

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

-spec do_fold_files(function(), function(), string(), [string()], any()) ->
  any().
do_fold_files(_F, _Filter,  _Dir, [], Acc0) ->
  Acc0;
do_fold_files(F, Filter, Dir, [File | Rest], Acc0) ->
  Path = filename:join(Dir, File),
  case not is_symlink(Path) of
    true  ->
      Acc = process_path(F, Filter, Path, Acc0),
      do_fold_files(F, Filter, Dir, Rest, Acc);
    false ->
      do_fold_files(F, Filter, Dir, Rest, Acc0)
  end.

-spec process_path(function(), function(), string(), any()) ->
  any().
process_path(F, Filter, Path, Acc) ->
  case filelib:is_regular(Path) of
    true ->
      case Filter(Path) of
        true  -> F(Path, Acc);
        false -> Acc
      end;
    false -> fold_files(F, Filter, Path, Acc)
  end.

-spec is_symlink(string()) -> boolean().
is_symlink(Path) ->
  case file:read_link(Path) of
    {ok, _} -> true;
    {error, _} -> false
  end.
