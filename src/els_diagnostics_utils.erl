%%==============================================================================
%% Diagnostics utils
%%==============================================================================
-module(els_diagnostics_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ dependencies/1
        , included_uris/1
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

-spec dependencies(uri()) -> [atom()].
dependencies(Uri) ->
  dependencies([Uri], [], sets:new()).

-spec included_uris(els_dt_document:item()) -> [uri()].
included_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  included_uris([Id || #{id := Id} <- POIs], []).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec dependencies([uri()], [atom()], sets:set(binary())) -> [atom()].
dependencies([], Acc, _AlreadyProcessed) ->
  Acc;
dependencies([Uri|Uris], Acc, AlreadyProcessed) ->
  case els_dt_document:lookup(Uri) of
    {ok, [Document]} ->
      Deps = els_dt_document:pois(Document, [behaviour, parse_transform]),
      IncludedUris = included_uris(Document),
      FilteredUris = [IncludedUri || IncludedUri <- IncludedUris,
                      not sets:is_element(IncludedUri, AlreadyProcessed)],
      dependencies(Uris ++ FilteredUris, Acc ++ [Id || #{id := Id} <- Deps],
                   sets:add_element(Uri, AlreadyProcessed));
    Error ->
      lager:info("Lookup failed [Error=~p]", [Error]),
      []
  end.

-spec included_uris([atom()], [uri()]) -> [uri()].
included_uris([], Acc) ->
  lists:usort(Acc);
included_uris([Id|Ids], Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> included_uris(Ids, [Uri | Acc]);
    {error, _Error} -> included_uris(Ids, Acc)
  end.
