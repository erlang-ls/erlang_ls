-module(els_references_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
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
is_enabled() ->
  true.

-spec handle_request(any(), state()) -> {[location()] | null, state()}.
handle_request({references, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  Refs =
    case
      els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1)
    of
      [POI | _] -> find_references(Uri, POI);
      []        -> []
    end,
  case Refs of
    [] -> {null, State};
    Rs -> {Rs, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(uri(), poi()) -> [location()].
find_references(Uri, #{ kind := Kind
                      , id   := Id
                      }) when Kind =:= application;
                              Kind =:= implicit_fun;
                              Kind =:= function;
                              Kind =:= export_entry;
                              Kind =:= export_type_entry;
                              Kind =:= type_application;
                              Kind =:= type_definition ->
  Key = case Id of
          {F, A}    -> {els_uri:module(Uri), F, A};
          {M, F, A} -> {M, F, A}
        end,
  find_references_for_id(Kind, Key);
find_references(Uri, #{ kind := Kind
                      , id   := Id
                      }) when Kind =:= function_clause ->
  {F, A, _Index} = Id,
  Key = {els_uri:module(Uri), F, A},
  find_references_for_id(Kind, Key);
find_references(_Uri, #{kind := Kind, id := Name})
  when Kind =:= record_expr;
       Kind =:= record ->
  find_references_for_id(Kind, Name);
find_references(_Uri, #{kind := Kind, id := Name})
  when Kind =:= macro;
       Kind =:= define ->
  find_references_for_id(Kind, Name);
find_references(Uri, #{kind := module}) ->
  case els_utils:lookup_document(Uri) of
    {ok, Doc} ->
      Exports = els_dt_document:pois(Doc, [export_entry]),
      ExcludeLocalRefs =
        fun(Loc) ->
            maps:get(uri, Loc) =/= Uri
        end,
      Refs = lists:flatmap(fun(E) -> find_references(Uri, E) end, Exports),
      lists:filter(ExcludeLocalRefs, Refs)
  end;
find_references(_Uri, #{kind := Kind, id := Name})
  when Kind =:= behaviour ->
  find_references_for_id(Kind, Name);
find_references(_Uri, _POI) ->
  [].

-spec find_references_for_id(poi_kind(), any()) -> [location()].
find_references_for_id(Kind, Id) ->
  {ok, Refs} = els_dt_references:find_by_id(Kind, Id),
  [location(U, R) || #{uri := U, range := R} <- Refs].

-spec location(uri(), poi_range()) -> location().
location(Uri, Range) ->
  #{ uri   => Uri
   , range => els_protocol:range(Range)
   }.
