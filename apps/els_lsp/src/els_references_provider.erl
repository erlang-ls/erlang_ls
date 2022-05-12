-module(els_references_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/2
]).

%% For use in other providers
-export([
    find_references/2,
    find_scoped_references_for_def/2,
    find_references_to_module/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Types
%%==============================================================================

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), any()) -> {response, [location()] | null}.
handle_request({references, Params}, _State) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    Refs =
        case els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1) of
            [POI | _] -> find_references(Uri, POI);
            [] -> []
        end,
    case Refs of
        [] -> {response, null};
        Rs -> {response, Rs}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(uri(), poi()) -> [location()].
find_references(Uri, #{
    kind := Kind,
    id := Id
}) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= function;
    Kind =:= export_entry;
    Kind =:= export_type_entry
->
    Key =
        case Id of
            {F, A} -> {els_uri:module(Uri), F, A};
            {M, F, A} -> {M, F, A}
        end,
    find_references_for_id(Kind, Key);
find_references(Uri, #{kind := variable} = Var) ->
    POIs = els_code_navigation:find_in_scope(Uri, Var),
    [location(Uri, Range) || #{range := Range} = POI <- POIs, POI =/= Var];
find_references(Uri, #{
    kind := Kind,
    id := Id
}) when Kind =:= function_clause ->
    {F, A, _Index} = Id,
    Key = {els_uri:module(Uri), F, A},
    find_references_for_id(Kind, Key);
find_references(Uri, Poi = #{kind := Kind}) when
    Kind =:= record;
    Kind =:= record_def_field;
    Kind =:= define
->
    uri_pois_to_locations(
        find_scoped_references_for_def(Uri, Poi)
    );
find_references(Uri, Poi = #{kind := Kind, id := Id}) when
    Kind =:= type_definition
->
    Key =
        case Id of
            {F, A} -> {els_uri:module(Uri), F, A};
            {M, F, A} -> {M, F, A}
        end,
    lists:usort(
        find_references_for_id(Kind, Key) ++
            uri_pois_to_locations(
                find_scoped_references_for_def(Uri, Poi)
            )
    );
find_references(Uri, Poi = #{kind := Kind}) when
    Kind =:= record_expr;
    Kind =:= record_field;
    Kind =:= macro;
    Kind =:= type_application
->
    case els_code_navigation:goto_definition(Uri, Poi) of
        {ok, DefUri, DefPoi} ->
            find_references(DefUri, DefPoi);
        {error, _} ->
            %% look for references only in the current document
            uri_pois_to_locations(
                find_scoped_references_for_def(Uri, Poi)
            )
    end;
find_references(Uri, #{kind := module}) ->
    Refs = find_references_to_module(Uri),
    [location(U, R) || #{uri := U, range := R} <- Refs];
find_references(_Uri, #{kind := Kind, id := Name}) when
    Kind =:= behaviour
->
    find_references_for_id(Kind, Name);
find_references(_Uri, _POI) ->
    [].

-spec find_scoped_references_for_def(uri(), poi()) -> [{uri(), poi()}].
find_scoped_references_for_def(Uri, #{kind := Kind, id := Name}) ->
    Kinds = kind_to_ref_kinds(Kind),
    Refs = els_scope:local_and_includer_pois(Uri, Kinds),
    [
        {U, Poi}
     || {U, Pois} <- Refs,
        #{id := N} = Poi <- Pois,
        N =:= Name
    ].

-spec kind_to_ref_kinds(poi_kind()) -> [poi_kind()].
kind_to_ref_kinds(define) ->
    [macro];
kind_to_ref_kinds(record) ->
    [record_expr];
kind_to_ref_kinds(record_def_field) ->
    [record_field];
kind_to_ref_kinds(type_definition) ->
    [type_application];
kind_to_ref_kinds(Kind) ->
    [Kind].

-spec find_references_to_module(uri()) -> [els_dt_references:item()].
find_references_to_module(Uri) ->
    M = els_uri:module(Uri),
    {ok, Doc} = els_utils:lookup_document(Uri),
    ExportRefs =
        lists:flatmap(
            fun(#{id := {F, A}}) ->
                {ok, Rs} =
                    els_dt_references:find_by_id(export_entry, {M, F, A}),
                Rs
            end,
            els_dt_document:pois(Doc, [export_entry])
        ),
    ExportTypeRefs =
        lists:flatmap(
            fun(#{id := {F, A}}) ->
                {ok, Rs} =
                    els_dt_references:find_by_id(export_type_entry, {M, F, A}),
                Rs
            end,
            els_dt_document:pois(Doc, [export_type_entry])
        ),
    {ok, BehaviourRefs} = els_dt_references:find_by_id(behaviour, M),
    ExcludeLocalRefs = fun(Loc) -> maps:get(uri, Loc) =/= Uri end,
    lists:filter(ExcludeLocalRefs, ExportRefs ++ ExportTypeRefs ++ BehaviourRefs).

-spec find_references_for_id(poi_kind(), any()) -> [location()].
find_references_for_id(Kind, Id) ->
    {ok, Refs} = els_dt_references:find_by_id(Kind, Id),
    [location(U, R) || #{uri := U, range := R} <- Refs].

-spec uri_pois_to_locations([{uri(), poi()}]) -> [location()].
uri_pois_to_locations(Refs) ->
    [location(U, R) || {U, #{range := R}} <- Refs].

-spec location(uri(), poi_range()) -> location().
location(Uri, Range) ->
    #{
        uri => Uri,
        range => els_protocol:range(Range)
    }.
