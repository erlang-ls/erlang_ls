-module(els_definition_provider).

-behaviour(els_provider).

-export([
    is_enabled/0,
    handle_request/2
]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {response, any()}.
handle_request({definition, Params}, State) ->
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
                    els_references_provider:handle_request({references, Params}, State);
                GoTo ->
                    {response, GoTo}
            end;
        GoTo ->
            {response, GoTo}
    end.

-spec goto_definition(uri(), [els_poi:poi()]) -> map() | null.
goto_definition(_Uri, []) ->
    null;
goto_definition(Uri, [POI | Rest]) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, DefUri, #{range := Range}} ->
            #{uri => DefUri, range => els_protocol:range(Range)};
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

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
fix_line_offset_test() ->
    In = #{range => #{from => {1, 16}, to => {1, 32}}},
    ?assertMatch(
        #{range := #{from := {66, 16}, to := {66, 32}}},
        fix_line_offset(In, 65)
    ).

-endif.
