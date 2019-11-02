-module(erlang_ls_utils).

-export([ find_module/1
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
  case erlang_ls_db:find(completion_index, M) of
    {ok, Uri} ->
      {ok, Uri};
    {error, not_found} ->
      FileName = atom_to_list(M) ++ ".erl",
      erlang_ls_index:find_and_index_file(FileName)
  end.

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
