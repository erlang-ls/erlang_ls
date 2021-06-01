%%==============================================================================
%% Unused Includes diagnostics
%%==============================================================================
-module(els_unused_includes_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case els_utils:lookup_document(Uri) of
    {error, _Error} ->
      [];
    {ok, Document} ->
      Includes = els_dt_document:pois(Document, [include, include_lib]),
      UnusedIncludes = find_unused_includes(Document, Includes),
      [ els_diagnostics:make_diagnostic(
          els_protocol:range(inclusion_range(UI, Document))
         , <<"Unused file: ", (filename:basename(UI))/binary>>
            , ?DIAGNOSTIC_WARNING
         , source()
         ) || UI <- UnusedIncludes ]
  end.

-spec source() -> binary().
source() ->
  <<"UnusedIncludes">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec find_unused_includes(els_dt_document:item(), [poi()]) -> [uri()].
find_unused_includes(#{uri := Uri} = Document, Includes) ->
  Graph = expand_includes(Document),
  POIs = els_dt_document:pois(Document),
  IncludedUris = els_diagnostics_utils:included_uris(Document),
  Fun = fun(POI, Acc) ->
            update_unused(Graph, Uri, POI, Acc)
        end,
  UnusedIncludes = lists:foldl(Fun, IncludedUris, POIs -- Includes),
  digraph:delete(Graph),
  UnusedIncludes.

-spec update_unused(digraph:graph(), uri(), poi(), [uri()]) -> [uri()].
update_unused(Graph, Uri, POI, Acc) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, Uri, _DefinitionPOI} ->
      Acc;
    {ok, DefinitionUri, _DefinitionPOI} ->
      case digraph:get_path(Graph, DefinitionUri, Uri) of
        false ->
          Acc;
        Path ->
          Acc -- Path
      end;
    {error, _Reason} ->
      Acc
  end.

-spec expand_includes(els_dt_document:item()) -> digraph:graph().
expand_includes(Document) ->
  DG = digraph:new(),
  AccFun = fun(#{uri := IncludedUri}, #{uri := IncluderUri}, _) ->
    Dest = digraph:add_vertex(DG, IncluderUri),
    Src = digraph:add_vertex(DG, IncludedUri),
    _IncludedBy = digraph:add_edge(DG, Src, Dest),
    DG
  end,
  els_diagnostics_utils:traverse_include_graph(AccFun, DG, Document).

-spec inclusion_range(uri(), els_dt_document:item()) -> poi_range().
inclusion_range(Uri, Document) ->
  Path = binary_to_list(els_uri:path(Uri)),
  case
    els_compiler_diagnostics:inclusion_range(Path, Document, include) ++
    els_compiler_diagnostics:inclusion_range(Path, Document, include_lib) of
    [Range|_] ->
      Range;
    _ ->
      #{from => {1, 1}, to => {2, 1}}
  end.
