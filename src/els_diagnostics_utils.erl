%%==============================================================================
%% Diagnostics utils
%%==============================================================================
-module(els_diagnostics_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ dependencies/1
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

-spec dependencies(uri()) -> [atom()].
dependencies(Uri) ->
  dependencies([Uri], []).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec dependencies([uri()], [atom()]) -> [atom()].
dependencies([], Acc) ->
  Acc;
dependencies([Uri|Uris], Acc) ->
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      els_dt_document:lookup(Uri),
      Deps = els_dt_document:pois(Document, [behaviour, parse_transform]),
      IncludedUris = included_uris(Document),
      dependencies(Uris ++ IncludedUris, Acc ++ [Id || #{id := Id} <- Deps]);
    Error ->
      lager:info("Lookup failed [Error=~p]", [Error]),
      []
  end.

-spec included_uris(els_dt_document:item()) -> [uri()].
included_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  included_uris([Id || #{id := Id} <- POIs], []).

-spec included_uris([atom()], [uri()]) -> [uri()].
included_uris([], Acc) ->
  lists:usort(Acc);
included_uris([Id|Ids], Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> included_uris(Ids, [Uri | Acc]);
    {error, _Error} -> included_uris(Ids, Acc)
  end.
