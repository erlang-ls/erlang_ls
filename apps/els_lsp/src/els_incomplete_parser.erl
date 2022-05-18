-module(els_incomplete_parser).
-export([parse_after/2]).
-export([parse_line/2]).

-include_lib("kernel/include/logger.hrl").

-spec parse_after(binary(), integer()) -> [els_poi:poi()].
parse_after(Text, Line) ->
    {_, AfterText} = els_text:split_at_line(Text, Line),
    {ok, POIs} = els_parser:parse(AfterText),
    POIs.

-spec parse_line(binary(), integer()) -> [els_poi:poi()].
parse_line(Text, Line) ->
    LineText0 = string:trim(els_text:line(Text, Line), trailing, ",;"),
    case els_parser:parse(LineText0) of
        {ok, []} ->
            LineStr = els_utils:to_list(LineText0),
            case lists:reverse(LineStr) of
                %% Kludge to parse "case foo() of"
                "fo " ++ _ ->
                    LineText1 = <<LineText0/binary, " _ -> _ end">>,
                    {ok, POIs} = els_parser:parse(LineText1),
                    POIs;
                _ ->
                    []
            end;
        {ok, POIs} ->
            POIs
    end.
