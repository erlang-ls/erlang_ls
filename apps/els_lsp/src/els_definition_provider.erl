-module(els_definition_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

-include("els_lsp.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()} | {async, uri(), pid()}.
handle_request({definition, Params}) ->
    #{
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        },
        <<"textDocument">> := #{<<"uri">> := Uri}
    } = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1),
    case goto_definition(Uri, POIs) of
        null ->
            #{text := Text} = Document,
            IncompletePOIs = match_incomplete(Text, {Line, Character}),
            case goto_definition(Uri, IncompletePOIs) of
                null ->
                    FuzzyPOIs = make_fuzzy(POIs),
                    case goto_definition(Uri, FuzzyPOIs) of
                        null ->
                            els_references_provider:handle_request({references, Params});
                        GoTo ->
                            {response, GoTo}
                    end;
                GoTo ->
                    {response, GoTo}
            end;
        GoTo ->
            {response, GoTo}
    end.

-spec make_fuzzy([els_poi:poi()]) -> [els_poi:poi()].
make_fuzzy(POIs) ->
    lists:flatmap(
        fun
            (#{kind := application, id := {M, F, _A}} = POI) ->
                [
                    POI#{id => {M, F, any_arity}, kind => application},
                    POI#{id => {M, F, any_arity}, kind => type_application}
                ];
            (#{kind := type_application, id := {M, F, _A}} = POI) ->
                [
                    POI#{id => {M, F, any_arity}, kind => type_application},
                    POI#{id => {M, F, any_arity}, kind => application}
                ];
            (#{kind := application, id := {F, _A}} = POI) ->
                [
                    POI#{id => {F, any_arity}, kind => application},
                    POI#{id => {F, any_arity}, kind => type_application},
                    POI#{id => {F, any_arity}, kind => macro},
                    POI#{id => F, kind => macro}
                ];
            (#{kind := type_application, id := {F, _A}} = POI) ->
                [
                    POI#{id => {F, any_arity}, kind => type_application},
                    POI#{id => {F, any_arity}, kind => application},
                    POI#{id => {F, any_arity}, kind => macro},
                    POI#{id => F, kind => macro}
                ];
            (#{kind := macro, id := {M, _A}} = POI) ->
                [
                    POI#{id => M},
                    POI#{id => {M, any_arity}}
                ];
            (#{kind := macro, id := M} = POI) ->
                [
                    POI#{id => {M, any_arity}}
                ];
            (#{kind := atom, id := Id} = POI) ->
                [
                    POI#{id => {Id, any_arity}, kind => application},
                    POI#{id => {Id, any_arity}, kind => type_application},
                    POI#{id => Id, kind => macro}
                ];
            (_POI) ->
                []
        end,
        POIs
    ).

-spec goto_definition(uri(), [els_poi:poi()]) -> [map()] | null.
goto_definition(_Uri, []) ->
    null;
goto_definition(Uri, [#{id := FunId, kind := function} = POI | Rest]) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    BehaviourPOIs = els_dt_document:pois(Document, [behaviour]),
    case BehaviourPOIs of
        [] ->
            %% cursor is not over a function - continue
            case els_code_navigation:goto_definition(Uri, POI) of
                {ok, Definitions} ->
                    goto_definitions_to_goto(Definitions);
                _ ->
                    goto_definition(Uri, Rest)
            end;
        Behaviours ->
            case does_implement_behaviour(FunId, Behaviours) of
                false ->
                    %% no matching callback for this behaviour so proceed
                    goto_definition(Uri, Rest);
                {true, BehaviourModuleUri, MatchingCallback} ->
                    {ok, Definitions} = els_code_navigation:goto_definition(
                        BehaviourModuleUri,
                        MatchingCallback
                    ),
                    goto_definitions_to_goto(Definitions)
            end
    end;
goto_definition(Uri, [POI | Rest]) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, Definitions} ->
            goto_definitions_to_goto(Definitions);
        _ ->
            goto_definition(Uri, Rest)
    end.

-spec match_incomplete(binary(), pos()) -> [els_poi:poi()].
match_incomplete(Text, {Line, Col} = Pos) ->
    %% Try parsing subsets of text to find a matching POI at Pos
    case match_after(Text, Pos) ++ match_line(Text, Pos) of
        [] ->
            %% Still found nothing, let's analyze the tokens to kludge a POI
            LineText = els_text:line(Text, Line),
            Tokens = els_text:tokens(LineText, {Line, 1}),
            kludge_match(Tokens, {Line, Col + 1});
        POIs ->
            POIs
    end.

-spec kludge_match([any()], pos()) -> [els_poi:poi()].
kludge_match([], _Pos) ->
    [];
kludge_match(
    [
        {atom, {FromL, FromC}, Module},
        {':', _},
        {atom, _, Function},
        {'(', {ToL, ToC}}
        | _
    ],
    {_, C}
) when
    FromC =< C, C < ToC
->
    %% Match mod:fun(
    Range = #{from => {FromL, FromC}, to => {ToL, ToC}},
    POI = els_poi:new(Range, application, {Module, Function, any_arity}),
    [POI];
kludge_match([{atom, {FromL, FromC}, Function}, {'(', {ToL, ToC}} | _], {_, C}) when
    FromC =< C, C < ToC
->
    %% Match fun(
    Range = #{from => {FromL, FromC}, to => {ToL, ToC}},
    POI = els_poi:new(Range, application, {Function, any_arity}),
    [POI];
kludge_match([{'#', _}, {atom, {FromL, FromC}, Record} | T], {_, C} = Pos) when
    FromC =< C
->
    %% Match #record
    ToC = FromC + length(atom_to_list(Record)),
    case C =< ToC of
        true ->
            Range = #{from => {FromL, FromC}, to => {FromL, ToC}},
            POI = els_poi:new(Range, record_expr, Record),
            [POI];
        false ->
            kludge_match(T, Pos)
    end;
kludge_match([{'?', _}, {VarOrAtom, {FromL, FromC}, Macro} | T], {_, C} = Pos) when
    FromC =< C, (VarOrAtom == var orelse VarOrAtom == atom)
->
    %% Match ?MACRO
    ToC = FromC + length(atom_to_list(Macro)),
    case C =< ToC of
        true ->
            %% Match fun(
            Range = #{from => {FromL, FromC}, to => {FromL, ToC}},
            POI = els_poi:new(Range, macro, Macro),
            [POI];
        false ->
            kludge_match(T, Pos)
    end;
kludge_match([{atom, {FromL, FromC}, Atom} | T], {_, C} = Pos) when
    FromC =< C
->
    %% Match atom
    ToC = FromC + length(atom_to_list(Atom)),
    case C =< ToC of
        true ->
            Range = #{from => {FromL, FromC}, to => {FromL, ToC}},
            POI = els_poi:new(Range, atom, Atom),
            [POI];
        false ->
            kludge_match(T, Pos)
    end;
kludge_match([_ | T], Pos) ->
    %% TODO: Add more kludges here
    kludge_match(T, Pos).

-spec match_after(binary(), pos()) -> [els_poi:poi()].
match_after(Text, {Line, Character}) ->
    %% Try to parse current line and the lines after it
    POIs = els_incomplete_parser:parse_after(Text, Line),
    MatchingPOIs = match_pois(POIs, {1, Character + 1}),
    fix_line_offsets(MatchingPOIs, Line).

-spec match_line(binary(), pos()) -> [els_poi:poi()].
match_line(Text, {Line, Character}) ->
    %% Try to parse only current line
    POIs = els_incomplete_parser:parse_line(Text, Line),
    MatchingPOIs = match_pois(POIs, {1, Character + 1}),
    fix_line_offsets(MatchingPOIs, Line).

-spec match_pois([els_poi:poi()], pos()) -> [els_poi:poi()].
match_pois(POIs, Pos) ->
    els_poi:sort(els_poi:match_pos(POIs, Pos)).

-spec fix_line_offsets([els_poi:poi()], integer()) -> [els_poi:poi()].
fix_line_offsets(POIs, Offset) ->
    [fix_line_offset(POI, Offset) || POI <- POIs].

-spec fix_line_offset(els_poi:poi(), integer()) -> els_poi:poi().
fix_line_offset(
    #{
        range := #{
            from := {FromL, FromC},
            to := {ToL, ToC}
        }
    } = POI,
    Offset
) ->
    %% TODO: Fix other ranges too
    POI#{
        range => #{
            from => {FromL + Offset, FromC},
            to => {ToL + Offset, ToC}
        }
    }.

-spec goto_definitions_to_goto(Definitions) -> Result when
    Definitions :: els_code_navigation:goto_definition(),
    Result :: [map()].
goto_definitions_to_goto(Definitions) ->
    lists:map(
        fun({DefUri, DefPOI}) ->
            #{range := Range} = DefPOI,
            #{uri => DefUri, range => els_protocol:range(Range)}
        end,
        Definitions
    ).

-spec does_implement_behaviour(any(), list()) -> {true, uri(), els_poi:poi()} | false.
does_implement_behaviour(_, []) ->
    false;
does_implement_behaviour(FunId, [#{id := ModuleId, kind := behaviour} | Rest]) ->
    {ok, BehaviourModuleUri} = els_utils:find_module(ModuleId),
    {ok, BehaviourModuleDocument} = els_utils:lookup_document(BehaviourModuleUri),
    DefinedCallbacks = els_dt_document:pois(
        BehaviourModuleDocument,
        [callback]
    ),
    MaybeMatchingCallback = lists:filter(
        fun(#{id := CallbackId}) ->
            CallbackId =:= FunId
        end,
        DefinedCallbacks
    ),
    case MaybeMatchingCallback of
        [] -> does_implement_behaviour(FunId, Rest);
        [H | _] -> {true, BehaviourModuleUri, H}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
fix_line_offset_test() ->
    In = #{range => #{from => {1, 16}, to => {1, 32}}},
    ?assertMatch(
        #{range := #{from := {66, 16}, to := {66, 32}}},
        fix_line_offset(In, 65)
    ).

-endif.
