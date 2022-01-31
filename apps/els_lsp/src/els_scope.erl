%%% @doc Library module to calculate various scoping rules
-module(els_scope).

-export([ local_and_included_pois/2
        , local_and_includer_pois/2
        , variable_scope_range/2
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

%% @doc Find the rough scope of a variable, this is based on heuristics and
%%      won't always be correct.
%%      `VarRange' is expected to be the range of the variable.
-spec variable_scope_range(poi_range(), els_dt_document:item()) -> poi_range().
variable_scope_range(VarRange, Document) ->
  Attributes = [spec, callback, define, record, type_definition],
  AttrPOIs = els_dt_document:pois(Document, Attributes),
  case pois_match(AttrPOIs, VarRange) of
    [#{range := Range}] ->
      %% Inside attribute, simple.
      Range;
    [] ->
      %% If variable is not inside an attribute we need to figure out where the
      %% scope of the variable begins and ends.
      %% The scope of variables inside functions are limited by function clauses
      %% The scope of variables outside of function are limited by top-level
      %% POIs (attributes and functions) before and after.
      FunPOIs = els_poi:sort(els_dt_document:pois(Document, [function])),
      POIs = els_poi:sort(els_dt_document:pois(Document, [ function_clause
                                                         | Attributes
                                                         ])),
      CurrentFunRange = case pois_match(FunPOIs, VarRange) of
                          [] -> undefined;
                          [POI] -> range(POI)
                        end,
      IsInsideFunction = CurrentFunRange /= undefined,
      BeforeFunRanges = [range(POI) || POI <- pois_before(FunPOIs, VarRange)],
      %% Find where scope should begin
      From =
        case [R || #{range := R} <- pois_before(POIs, VarRange)] of
          [] ->
            %% No POIs before
            {0, 0};
          [BeforeRange|_] when IsInsideFunction ->
            %% Inside function, use beginning of closest function clause
            maps:get(from, BeforeRange);
          [BeforeRange|_] when BeforeFunRanges == [] ->
            %% No function before, use end of closest POI
            maps:get(to, BeforeRange);
          [BeforeRange|_] ->
            %% Use end of closest POI, including functions.
            max(maps:get(to, hd(BeforeFunRanges)),
                maps:get(to, BeforeRange))
        end,
      %% Find when scope should end
      To =
        case [R || #{range := R} <- pois_after(POIs, VarRange)] of
          [] when IsInsideFunction ->
            %% No POIs after, use end of function
            maps:get(to, CurrentFunRange);
          [] ->
            %% No POIs after, use end of document
            {999999999, 999999999};
          [AfterRange|_] when IsInsideFunction ->
            %% Inside function, use closest of end of function *OR*
            %% beginning of the next function clause
            min(maps:get(to, CurrentFunRange), maps:get(from, AfterRange));
          [AfterRange|_] ->
            %% Use beginning of next POI
            maps:get(from, AfterRange)
        end,
      #{from => From, to => To}
  end.

-spec pois_before([poi()], poi_range()) -> [poi()].
pois_before(POIs, VarRange) ->
  %% Reverse since we are typically interested in the last POI
  lists:reverse([POI || POI <- POIs, els_range:compare(range(POI), VarRange)]).

-spec pois_after([poi()], poi_range()) -> [poi()].
pois_after(POIs, VarRange) ->
  [POI || POI <- POIs, els_range:compare(VarRange, range(POI))].

-spec pois_match([poi()], poi_range()) -> [poi()].
pois_match(POIs, Range) ->
  [POI || POI <- POIs, els_range:in(Range, range(POI))].

-spec range(poi()) -> poi_range().
range(#{kind := function, data := #{wrapping_range := Range}}) ->
  Range;
range(#{range := Range}) ->
  Range.
