%%==============================================================================
%% Diagnostics utils
%%==============================================================================
-module(els_diagnostics_utils).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ dependencies/1
        , included_uris/1
        , included_documents/1
        , range/2
        , traverse_include_graph/3
        ]).
%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-spec dependencies(uri()) -> [atom()].
dependencies(Uri) ->
  dependencies([Uri], [], sets:new()).

-spec included_uris(els_dt_document:item()) -> [uri()].
included_uris(Document) ->
  POIs = els_dt_document:pois(Document, [include, include_lib]),
  included_uris([Id || #{id := Id} <- POIs], []).

-spec included_documents(els_dt_document:item()) -> [els_dt_document:item()].
included_documents(Document) ->
  lists:filtermap(fun find_included_document/1, included_uris(Document)).

-spec find_included_document(uri()) -> {true, els_dt_document:item()} | false.
find_included_document(Uri) ->
  case els_utils:lookup_document(Uri) of
    {ok, IncludeDocument} ->
      {true, IncludeDocument};
    {error, _} ->
      ?LOG_WARNING("Failed included document lookup [uri=~p]", [Uri]),
      false
  end.

-spec range(els_dt_document:item() | undefined,
            erl_anno:anno() | none) -> poi_range().
range(Document, none) ->
  range(Document, erl_anno:new(1));
range(Document, Anno) ->
  true = erl_anno:is_anno(Anno),
  Line = erl_anno:line(Anno),
  case erl_anno:column(Anno) of
    Col when Document =:= undefined; Col =:= undefined ->
      #{from => {Line, 1}, to => {Line + 1, 1}};
    Col ->
      POIs0 = els_dt_document:get_element_at_pos(Document, Line, Col),

      %% Exclude folding range since line is more exact anyway
      POIs  = [POI || #{kind := Kind} = POI <- POIs0, Kind =/= folding_range],

      %% * If we find no pois that we just return the original line
      %% * If we find a poi that start on the line and col as the anno
      %%   we are looking for we that that one.
      %% * We take the "first" poi if we find some, but none come from
      %%   the correct line and number.

      case lists:search(
             fun(#{ range := #{ from := {FromLine, FromCol} } }) ->
                 FromLine =:= Line andalso FromCol =:= Col
             end, POIs) of
        {value, #{ range := Range } } ->
          Range;
        false when POIs =:= [] ->
          #{ from => {Line, 1}, to => {Line + 1, 1} };
        false ->
          maps:get(range, hd(POIs))
      end
  end.

-spec traverse_include_graph(AccFun, AccT, From) -> AccT when
    AccFun :: fun((Included, Includer, AccT) -> AccT),
    From :: els_dt_document:item(),
    Included :: els_dt_document:item(),
    Includer :: els_dt_document:item().
traverse_include_graph(AccFun, Acc, From) ->
    Graph = els_fungraph:new(
      fun els_dt_document:uri/1,
      fun included_documents/1),
    els_fungraph:traverse(AccFun, Acc, From, Graph).

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec dependencies([uri()], [atom()], sets:set(binary())) -> [atom()].
dependencies([], Acc, _AlreadyProcessed) ->
  Acc;
dependencies([Uri|Uris], Acc, AlreadyProcessed) ->
  case els_utils:lookup_document(Uri) of
    {ok, Document} ->
      Behaviours = els_dt_document:pois(Document, [behaviour]),
      ParseTransforms = els_dt_document:pois(Document, [parse_transform]),
      IncludedUris = included_uris(Document),
      FilteredIncludedUris = exclude_already_processed( IncludedUris
                                                      , AlreadyProcessed
                                                      ),
      PTUris = lists:usort(
                 lists:flatten(
                   [pt_deps(Id) || #{id := Id} <- ParseTransforms])),
      FilteredPTUris = exclude_already_processed( PTUris
                                                , AlreadyProcessed
                                                ),
      dependencies( Uris ++ FilteredIncludedUris ++ FilteredPTUris
                  , Acc ++ [Id || #{id := Id} <- Behaviours ++ ParseTransforms]
                    ++ [els_uri:module(FPTUri) || FPTUri <- FilteredPTUris]
                  , sets:add_element(Uri, AlreadyProcessed));
    {error, _Error} ->
      []
  end.

-spec exclude_already_processed([uri()], sets:set()) -> [uri()].
exclude_already_processed(Uris, AlreadyProcessed) ->
  [Uri || Uri <- Uris, not sets:is_element(Uri, AlreadyProcessed)].

-spec pt_deps(atom()) -> [uri()].
pt_deps(Module) ->
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      case els_utils:lookup_document(Uri) of
        {ok, Document} ->
          Applications = els_dt_document:pois(Document, [ application
                                                        , implicit_fun
                                                        ]),
          applications_to_uris(Applications);
        {error, _Error} ->
          []
      end;
    {error, Error} ->
      ?LOG_INFO("Find module failed [module=~p] [error=~p]", [Module, Error]),
      []
  end.

-spec applications_to_uris([poi()]) -> [uri()].
applications_to_uris(Applications) ->
  Modules = [M || #{id := {M, _F, _A}} <- Applications],
  Fun = fun(M, Acc) ->
            case els_utils:find_module(M) of
              {ok, Uri} ->
                [Uri|Acc];
              {error, Error} ->
                ?LOG_INFO( "Could not find module [module=~p] [error=~p]"
                         , [M, Error]
                         ),
                Acc
            end
        end,
  lists:foldl(Fun, [], Modules).

-spec included_uris([atom()], [uri()]) -> [uri()].
included_uris([], Acc) ->
  lists:usort(Acc);
included_uris([Id|Ids], Acc) ->
  case els_utils:find_header(els_utils:filename_to_atom(Id)) of
    {ok, Uri}       -> included_uris(Ids, [Uri | Acc]);
    {error, _Error} -> included_uris(Ids, Acc)
  end.
