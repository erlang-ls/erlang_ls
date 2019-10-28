-module(erlang_ls_utils).

-export([ find_module/1
        , halt/1
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

-spec halt(integer()) -> no_return().
halt(ExitCode) ->
  erlang:halt(ExitCode).
