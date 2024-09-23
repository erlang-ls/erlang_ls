-module(els_references_provider).

-behaviour(els_provider).

-export([
    handle_request/1
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
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {async, uri(), pid()}.
handle_request({references, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    ?LOG_DEBUG(
        "Starting references job " "[uri=~p, line=~p, character=~p]",
        [Uri, Line, Character]
    ),
    Job = run_references_job(Uri, Line, Character),
    {async, Uri, Job}.

-spec run_references_job(uri(), line(), column()) -> pid().
run_references_job(Uri, Line, Character) ->
    Config = #{
        task => fun get_references/2,
        entries => [{Uri, Line, Character}],
        title => <<"References">>,
        on_complete => fun els_server:register_result/1
    },
    {ok, Pid} = els_background_job:new(Config),
    Pid.

-spec get_references({uri(), integer(), integer()}, _) -> null | [location()].
get_references({Uri, Line, Character}, _) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    Refs =
        case els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1) of
            [POI | _] -> find_references(Uri, POI);
            [] -> []
        end,
    case Refs of
        [] -> null;
        Rs -> Rs
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_references(uri(), els_poi:poi()) -> [location()].
find_references(Uri, #{
    kind := Kind,
    id := Id
}) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= function;
    Kind =:= export_entry;
    Kind =:= export_type_entry;
    Kind =:= nifs_entry
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
find_references(Uri, POI = #{kind := Kind}) when
    Kind =:= record;
    Kind =:= record_def_field;
    Kind =:= define
->
    find_scoped_references_for_def(Uri, POI);
find_references(Uri, POI = #{kind := Kind, id := Id}) when
    Kind =:= type_definition
->
    Key =
        case Id of
            {F, A} -> {els_uri:module(Uri), F, A};
            {M, F, A} -> {M, F, A}
        end,
    lists:usort(
        find_references_for_id(Kind, Key) ++
            find_scoped_references_for_def(Uri, POI)
    );
find_references(Uri, POI = #{kind := Kind, id := Id}) when
    Kind =:= record_expr;
    Kind =:= record_field;
    Kind =:= macro;
    Kind =:= type_application
->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, [{DefUri, DefPoi}]} ->
            find_references(DefUri, DefPoi);
        _ ->
            %% look for references only in the current document
            local_refs(Uri, Kind, Id)
    end;
find_references(Uri, #{kind := module}) ->
    Refs = find_references_to_module(Uri),
    [location(U, R) || #{uri := U, range := R} <- Refs];
find_references(_Uri, #{kind := Kind, id := Name}) when
    Kind =:= behaviour;
    Kind =:= atom
->
    find_references_for_id(Kind, Name);
find_references(_Uri, _POI) ->
    [].

-spec local_refs(uri(), els_poi:poi_kind(), els_poi:poi_id()) ->
    [location()].
local_refs(Uri, Kind, Id) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Document, [kind_to_ref_kind(Kind)]),
    LocalRefs = [
        location(Uri, R)
     || #{range := R, id := IdPoi} <- POIs,
        Id == IdPoi
    ],
    LocalRefs.

-spec find_scoped_references_for_def(uri(), els_poi:poi()) -> [location()].
find_scoped_references_for_def(Uri, POI = #{kind := Kind}) when
    Kind =:= type_definition
->
    %% TODO: This is a hack, ideally we shouldn't have any special handling for
    %%       these kinds.
    find_scoped_references_naive(Uri, POI);
find_scoped_references_for_def(Uri, POI) ->
    %% Finding scoped references can be done in two ways:
    %% * Naive, find all POIs that can reach our POI and matches the id.
    %% * Indexed, use the index to find all matching POIs, then check if
    %%   they actually reference our POI by using goto_definition.
    %% It varies from case to case which is the fastest, so we race both
    %% functions to get the quickest answer.
    Naive = fun() -> find_scoped_references_naive(Uri, POI) end,
    Index = fun() -> find_scoped_references_with_index(Uri, POI) end,
    els_utils:race([Naive, Index], _Timeout = 15000).

-spec find_scoped_references_naive(uri(), els_poi:poi()) -> [location()].
find_scoped_references_naive(Uri, #{id := Id, kind := Kind}) ->
    RefKind = kind_to_ref_kind(Kind),
    Refs = els_scope:local_and_includer_pois(Uri, [RefKind]),
    MatchingRefs = [
        location(U, R)
     || {U, POIs} <- Refs,
        #{id := N, range := R} <- POIs,
        N =:= Id
    ],
    ?LOG_DEBUG(
        "Found scoped references (naive) for ~p: ~p",
        [Id, length(MatchingRefs)]
    ),
    MatchingRefs.

-spec find_scoped_references_with_index(uri(), els_poi:poi()) -> [location()].
find_scoped_references_with_index(Uri, POI = #{kind := Kind, id := Id}) ->
    RefPOI = POI#{kind := kind_to_ref_kind(Kind)},
    Match = fun(#{uri := RefUri}) ->
        case els_code_navigation:goto_definition(RefUri, RefPOI) of
            {ok, [{Uri, _}]} -> true;
            _Else -> false
        end
    end,
    Refs = [Ref || Ref <- find_references_for_id(Kind, Id), Match(Ref)],
    ?LOG_DEBUG(
        "Found scoped references (with index) for ~p: ~p",
        [Id, length(Refs)]
    ),
    Refs.

-spec kind_to_ref_kind(els_poi:poi_kind()) -> els_poi:poi_kind().
kind_to_ref_kind(define) ->
    macro;
kind_to_ref_kind(record) ->
    record_expr;
kind_to_ref_kind(record_def_field) ->
    record_field;
kind_to_ref_kind(type_definition) ->
    type_application;
kind_to_ref_kind(Kind) ->
    Kind.

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

-spec find_references_for_id(els_poi:poi_kind(), any()) -> [location()].
find_references_for_id(Kind, Id) ->
    {ok, Refs} = els_dt_references:find_by_id(Kind, Id),
    [location(U, R) || #{uri := U, range := R} <- Refs].

-spec location(uri(), els_poi:poi_range()) -> location().
location(Uri, Range) ->
    #{
        uri => Uri,
        range => els_protocol:range(Range)
    }.
