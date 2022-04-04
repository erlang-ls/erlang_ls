%%==============================================================================
%% Text-based search
%%==============================================================================
-module(els_text_search).

%%==============================================================================
%% API
%%==============================================================================
-export([ find_candidate_uris/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec find_candidate_uris({els_dt_references:poi_category(), any()}) -> [uri()].
find_candidate_uris(Id) ->
  Cmd = search_command(greppable_string(Id)),
  ?LOG_DEBUG("Finding candidate uris via: ~p", [Cmd]),
  Result = string:trim(os:cmd(Cmd), trailing),
  ?LOG_DEBUG("Candidates found: ~p", [Result]),
  Candidates = string:split(Result, "\n", all),
  [els_uri:uri(els_utils:to_binary(Candidate)) || Candidate <- Candidates].

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec searchable_paths() -> [string()].
searchable_paths() ->
  els_config:get(apps_paths) ++ els_config:get(deps_paths).

-spec search_command(string()) -> string().
search_command(IdString) ->
  SearchablePaths = [io_lib:format("~p", [Path]) || Path <- searchable_paths()],
  PathsString = string:join(SearchablePaths, " "),
  CmdFormat = els_config:get(search_command),
  CmdArgs = [IdString, PathsString],
  lists:flatten(io_lib:format(CmdFormat, CmdArgs)).

-spec greppable_string({els_dt_references:poi_category(), any()}) ->
        {ok, string()}.
greppable_string({function, {_M, F, _A}}) ->
  atom_to_greppable_string(F);
greppable_string({type, {_M, F, _A}}) ->
  atom_to_greppable_string(F);
greppable_string({macro, {Name, _Arity}}) ->
  atom_to_greppable_string(Name);
greppable_string({macro, Name}) ->
  atom_to_greppable_string(Name);
greppable_string({include, Id}) ->
  include_id_to_greppable_string(Id);
greppable_string({include_lib, Id}) ->
  include_id_to_greppable_string(Id);
greppable_string({behaviour, Name}) ->
  atom_to_greppable_string(Name).

-spec atom_to_greppable_string(atom()) -> string().
atom_to_greppable_string(Atom) ->
  lists:flatten(io_lib:format("~p", [Atom])).

-spec include_id_to_greppable_string(string()) -> string().
include_id_to_greppable_string(Id) ->
  filename:rootname(filename:basename(Id)).
