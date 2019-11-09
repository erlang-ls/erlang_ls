-module(els_utils).

-export([ find_document/1
        , find_module/1
        , find_module/2
        , include_filename/2
        , halt/1
        , start_epmd/0
        ]).

-include("erlang_ls.hrl").

%% @doc Look for a module in the DB.
%%
%% Look for a given module in the DB and return the respective
%% `URI`. If the module is not in the DB, try to index it.

-spec find_module(atom()) -> {ok, uri()} | {error, any()}.
find_module(M) ->
  find_module(M, erl).

-spec find_module(atom(), erl | hrl) -> {ok, uri()} | {error, any()}.
find_module(M, Extension) ->
  case els_db:find(modules, M) of
    {ok, Uri} ->
      {ok, Uri};
    {error, not_found} ->
      FileName = atom_to_list(M) ++ extension(Extension),
      els_indexer:find_and_index_file(FileName, sync)
  end.

-spec find_document(uri()) ->
  {ok, els_document:document()} | {error, any()}.
find_document(Uri) ->
  case els_db:find(documents, Uri) of
    {ok, Document} ->
      {ok, Document};
    {error, not_found} ->
      Path = els_uri:path(Uri),
      els_indexer:index_file(Path, sync)
  end.

-spec extension(erl | hrl) -> string().
extension(erl) -> ".erl";
extension(hrl) -> "".

-spec include_filename('include' | 'include_lib', string()) -> string().
include_filename(include, String) ->
  string:trim(String, both, [$"]);
include_filename(include_lib, String) ->
  lists:last(filename:split(string:trim(String, both, [$"]))).

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  erlang:halt(ExitCode).

-spec start_epmd() -> ok.
start_epmd() ->
    [] = os:cmd(epmd_path() ++ " -daemon"),
    ok.

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
