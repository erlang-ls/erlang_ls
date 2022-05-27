-module(els_document_highlight_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {response, any()}.
handle_request({document_highlight, Params}, _State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  Highlights = case
      els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1)
    of
      [POI | _] -> find_highlights(Document, POI);
      []        -> null
    end,
  case {Highlights, wrangler_handler:get_highlights(Uri, Line, Character)} of
    {H, null} -> {response, H};
    {_, H} -> {response, H}
              %% overwrites them for more transparent Wrangler forms.
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_highlights(els_dt_document:item(), els_poi:poi()) -> any().
find_highlights(Document, #{id := Id, kind := atom}) ->
    AtomHighlights = do_find_highlights(Document, Id, [atom]),
    FieldPOIs = els_dt_document:pois(Document, [
        record_def_field,
        record_field
    ]),
    FieldHighlights = [
        document_highlight(R)
     || #{id := I, range := R} <- FieldPOIs,
        element(2, I) =:= Id
    ],
    normalize_result(AtomHighlights ++ FieldHighlights);
find_highlights(Document, #{id := Id, kind := Kind}) ->
    POIs = els_dt_document:pois(Document, find_similar_kinds(Kind)),
    Highlights = [
        document_highlight(R)
     || #{id := I, range := R} <- POIs,
        I =:= Id
    ],
    normalize_result(Highlights).

-spec do_find_highlights(els_dt_document:item(), els_poi:poi_id(), [els_poi:poi_kind()]) ->
    any().
do_find_highlights(Document, Id, Kinds) ->
    POIs = els_dt_document:pois(Document, Kinds),
    _Highlights = [
        document_highlight(R)
     || #{id := I, range := R} <- POIs,
        I =:= Id
    ].

-spec document_highlight(els_poi:poi_range()) -> map().
document_highlight(Range) ->
    #{
        range => els_protocol:range(Range),
        kind => ?DOCUMENT_HIGHLIGHT_KIND_TEXT
    }.

-spec normalize_result([map()]) -> [map()] | null.
normalize_result([]) ->
    null;
normalize_result(L) when is_list(L) ->
    L.

-spec find_similar_kinds(els_poi:poi_kind()) -> [els_poi:poi_kind()].
find_similar_kinds(Kind) ->
    find_similar_kinds(Kind, kind_groups()).

-spec find_similar_kinds(els_poi:poi_kind(), [[els_poi:poi_kind()]]) -> [els_poi:poi_kind()].
find_similar_kinds(Kind, []) ->
    [Kind];
find_similar_kinds(Kind, [Group | Groups]) ->
    case lists:member(Kind, Group) of
        true ->
            Group;
        false ->
            find_similar_kinds(Kind, Groups)
    end.

%% Each group represents a list of POI kinds which represent the same or similar
%% objects (usually the definition and the usages of an object). Each POI kind
%% in one group must have the same id format.
-spec kind_groups() -> [[els_poi:poi_kind()]].
kind_groups() ->
    %% function
    [
        [
            application,
            implicit_fun,
            function,
            export_entry
        ],
        %% record
        [
            record,
            record_expr
        ],
        %% record_field
        [
            record_def_field,
            record_field
        ],
        %% macro
        [
            define,
            macro
        ]
    ].
