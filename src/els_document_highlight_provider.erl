-module(els_document_highlight_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_highlight, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  case
    els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI | _] -> {find_highlights(Document, POI), State};
    []        -> {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_highlights(els_dt_document:item(), poi()) -> any().
find_highlights(Document, #{ id := Id, kind := Kind })
  when Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= function;
       Kind =:= export_entry ->
  do_find_highlights(Document, Id, [ application
                                   , implicit_fun
                                   , function
                                   , export_entry]);
find_highlights(Document, #{ id := Id, kind := Kind })
  when Kind =:= record_def_field;
       Kind =:= record_field ->
  do_find_highlights(Document, Id, [ record_def_field
                                   , record_field]);
find_highlights(Document, #{ id := Id, kind := Kind }) ->
  POIs = els_dt_document:pois(Document, [Kind]),
  Highlights = [document_highlight(R) ||
                 #{id := I, kind := K, range := R} <- POIs,
                 I =:= Id,
                 K =/= 'folding_range'
               ],
  normalize_result(Highlights).

-spec do_find_highlights(els_dt_document:item() , poi_id() , [poi_kind()])
                        -> any().
do_find_highlights(Document, Id, Kinds) ->
  POIs = els_dt_document:pois(Document, Kinds),
  Highlights = [document_highlight(R) ||
                 #{id := I, range := R} <- POIs,
                 I =:= Id
               ],
  normalize_result(Highlights).

-spec document_highlight(poi_range()) -> map().
document_highlight(Range) ->
  #{ range => els_protocol:range(Range)
   , kind => ?DOCUMENT_HIGHLIGHT_KIND_TEXT
   }.

-spec normalize_result([map()]) -> [map()] | null.
normalize_result([]) ->
  null;
normalize_result(L) when is_list(L) ->
  L.
