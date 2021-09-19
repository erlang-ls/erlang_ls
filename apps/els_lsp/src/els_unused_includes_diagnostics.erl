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
      UnusedIncludes = find_unused_includes(Document),
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
-spec find_unused_includes(els_dt_document:item()) -> [uri()].
find_unused_includes(#{uri := Uri} = Document) ->
  Graph = expand_includes(Document),
  POIs = els_dt_document:pois(Document,
    [ application
    , implicit_fun
    , import_entry
    , macro
    , record_expr
    , record_field
    , type_application
    , export_type_entry
    ]),
  IncludedUris0 = els_diagnostics_utils:included_uris(Document),
  IncludedUris1 = filter_includes_with_compiler_attributes(IncludedUris0),
  ExcludeUnusedIncludes =
      lists:filtermap(
          fun(Include) ->
              case els_utils:find_header(els_utils:filename_to_atom(Include)) of
                  {ok, File} -> {true, File};
                  {error, _Error} ->
                      false
              end
          end, els_config:get(exclude_unused_includes)),
  IncludedUris = IncludedUris1 -- ExcludeUnusedIncludes,
  Fun = fun(POI, Acc) ->
            update_unused(Graph, Uri, POI, Acc)
        end,
  UnusedIncludes = lists:foldl(Fun, IncludedUris, POIs),
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

%% @doc Given a list of included uris, filter out the ones containing
%% compiler attributes. If the Uri cannot be found, keep it in the list.
-spec filter_includes_with_compiler_attributes([uri()]) -> [uri()].
filter_includes_with_compiler_attributes(Uris) ->
  Filter = fun(Uri) ->
               case els_utils:lookup_document(Uri) of
                 {error, _Error} ->
                   {true, Uri};
                 {ok, Document} ->
                    case contains_compiler_attributes(Document) of
                      true ->
                        false;
                      false ->
                        {true, Uri}
                    end
               end
           end,
  lists:filtermap(Filter, Uris).

%% @doc Return true if the Document contains a compiler attribute.
-spec contains_compiler_attributes(els_dt_document:item()) -> boolean().
contains_compiler_attributes(Document) ->
  compiler_attributes(Document) =/= [].

-spec compiler_attributes(els_dt_document:item()) -> [poi()].
compiler_attributes(Document) ->
  els_dt_document:pois(Document, [compile]).
