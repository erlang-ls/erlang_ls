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
                    els_references_provider:handle_request({references, Params});
                GoTo ->
                    {response, GoTo}
            end;
        GoTo ->
            {response, GoTo}
    end.

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
match_incomplete(Text, Pos) ->
    %% Try parsing subsets of text to find a matching POI at Pos
    match_after(Text, Pos) ++ match_line(Text, Pos).

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
