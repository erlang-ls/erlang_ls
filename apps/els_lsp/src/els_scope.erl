%%% @doc Library module to calculate various scoping rules
-module(els_scope).

-export([ local_and_included_pois/2
        , local_and_includer_pois/2
        ]).

-include("els_lsp.hrl").

%% @doc Return POIs of the provided `Kinds' in the document and included files
-spec local_and_included_pois(els_dt_document:item(), poi_kind() | [poi_kind()])
                             -> [poi()].
local_and_included_pois(Document, Kind) when is_atom(Kind) ->
  local_and_included_pois(Document, [Kind]);
local_and_included_pois(Document, Kinds) ->
  lists:flatten([ els_dt_document:pois(Document, Kinds)
                , included_pois(Document, Kinds)
                ]).

%% @doc Return POIs of the provided `Kinds' in included files from `Document'
-spec included_pois(els_dt_document:item(), [poi_kind()]) -> [poi()].
included_pois(Document, Kinds) ->
  els_diagnostics_utils:traverse_include_graph(
    fun(IncludedDocument, _Includer, Acc) ->
      els_dt_document:pois(IncludedDocument, Kinds) ++ Acc
    end,
    [],
    Document).

%% @doc Return POIs of the provided `Kinds' in the local document and files that
%% (maybe recursively) include it
-spec local_and_includer_pois(uri(), [poi_kind()]) ->
        [{uri(), [poi()]}].
local_and_includer_pois(LocalUri, Kinds) ->
  [{Uri, find_pois_by_uri(Uri, Kinds)}
   || Uri <- local_and_includers(LocalUri)].

-spec find_pois_by_uri(uri(), [poi_kind()]) -> [poi()].
find_pois_by_uri(Uri, Kinds) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  els_dt_document:pois(Document, Kinds).

-spec local_and_includers(uri()) -> [uri()].
local_and_includers(Uri) ->
  find_includers_loop(Uri, [Uri]).

-spec find_includers_loop(uri(), ordsets:ordset(uri())) ->
        ordsets:ordset(uri()).
find_includers_loop(Uri, Acc0) ->
  Includers = find_includers(Uri),
  case ordsets:subtract(Includers, Acc0) of
    [] ->
      %% no new uris
      Acc0;
    New ->
      Acc1 = ordsets:union(New, Acc0),
      lists:foldl(fun find_includers_loop/2, Acc1, New)
  end.

-spec find_includers(uri()) -> [uri()].
find_includers(Uri) ->
  IncludeId = els_utils:include_id(els_uri:path(Uri)),
  IncludeLibId = els_utils:include_lib_id(els_uri:path(Uri)),
  lists:usort(find_includers(include, IncludeId) ++
                find_includers(include_lib, IncludeLibId)).

-spec find_includers(poi_kind(), string()) -> [uri()].
find_includers(Kind, Id) ->
  {ok, Items} = els_dt_references:find_by_id(Kind, Id),
  [Uri || #{uri := Uri} <- Items].
