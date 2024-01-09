%%==============================================================================
%% Text Manipulation Functions
%%==============================================================================
-module(els_text).

-export([
    last_token/1,
    line/2,
    line/3,
    get_char/3,
    range/3,
    split_at_line/2,
    tokens/1,
    apply_edits/2
]).
-export([strip_comments/1]).

-export_type([edit/0]).

-type edit() :: {els_poi:poi_range(), string()}.
-type lines() :: [string() | binary()].
-type text() :: binary().
-type line_num() :: non_neg_integer().
-type column_num() :: pos_integer().
-type token() :: erl_scan:token().

%% @doc Extract the N-th line from a text.
-spec line(text(), line_num()) -> text().
line(Text, LineNum) ->
    Lines = binary:split(Text, [<<"\r\n">>, <<"\n">>], [global]),
    lists:nth(LineNum + 1, Lines).

%% @doc Extract the N-th line from a text, up to the given column number.
-spec line(text(), line_num(), column_num()) -> text().
line(Text, LineNum, ColumnNum) ->
    Line = line(Text, LineNum),
    binary:part(Line, {0, ColumnNum}).

-spec get_char(text(), line_num(), column_num()) ->
    {ok, char()} | {error, out_of_range}.
get_char(Text, Line, Column) ->
    LineStarts = line_starts(Text),
    Pos = pos(LineStarts, {Line, Column}),
    case Pos < size(Text) of
        true ->
            {ok, binary:at(Text, Pos)};
        false ->
            {error, out_of_range}
    end.

%% @doc Extract a snippet from a text, from [StartLoc..EndLoc).
-spec range(text(), {line_num(), column_num()}, {line_num(), column_num()}) ->
    text().
range(Text, StartLoc, EndLoc) ->
    LineStarts = line_starts(Text),
    StartPos = pos(LineStarts, StartLoc),
    EndPos = pos(LineStarts, EndLoc),
    binary:part(Text, StartPos, EndPos - StartPos).

-spec split_at_line(text(), line_num()) -> {text(), text()}.
split_at_line(Text, Line) ->
    StartPos = pos(line_starts(Text), {Line + 1, 1}),
    <<Left:StartPos/binary, Right/binary>> = Text,
    {Left, Right}.

%% @doc Return tokens from text.
-spec tokens(text()) -> [any()].
tokens(Text) ->
    case erl_scan:string(els_utils:to_list(Text)) of
        {ok, Tokens, _} -> Tokens;
        {error, _, _} -> []
    end.

%% @doc Extract the last token from the given text.
-spec last_token(text()) -> token() | {error, empty}.
last_token(Text) ->
    case tokens(Text) of
        [] -> {error, empty};
        Tokens -> lists:last(Tokens)
    end.

-spec apply_edits(text(), [edit()]) -> text().
apply_edits(Text, []) ->
    Text;
apply_edits(Text, Edits) when is_binary(Text) ->
    lists:foldl(
        fun(Edit, Acc) ->
            lines_to_bin(apply_edit(bin_to_lines(Acc), 0, Edit))
        end,
        Text,
        Edits
    ).

-spec apply_edit(lines(), line_num(), edit()) -> lines().
apply_edit([], L, {#{from := {FromL, _}}, _} = Edit) when L < FromL ->
    %% End of lines
    %% Add empty line
    [[] | apply_edit([], L + 1, Edit)];
apply_edit([], L, {#{from := {L, FromC}}, Insert}) ->
    %% End of lines
    Padding = lists:duplicate(FromC, $\s),
    string:split(Padding ++ Insert, "\n");
apply_edit([CurrLine | RestLines], L, {#{from := {FromL, _}}, _} = Edit) when
    L < FromL
->
    %% Go to next line
    [CurrLine | apply_edit(RestLines, L + 1, Edit)];
apply_edit(
    [CurrLine0 | RestLines],
    L,
    {#{from := {L, FromC}, to := {L, ToC}}, Insert}
) ->
    CurrLine = ensure_string(CurrLine0),
    %% One line edit
    {Prefix, Rest} = lists:split(FromC, CurrLine),
    {_, Suffix} = lists:split(ToC - FromC, Rest),
    string:split(Prefix ++ Insert ++ Suffix, "\n") ++ RestLines;
apply_edit(
    [CurrLine0 | RestLines],
    L,
    {#{from := {L, FromC}, to := {ToL, ToC}}, Insert}
) ->
    %% Multiline edit
    CurrLine = ensure_string(CurrLine0),
    {Prefix, _} = lists:split(FromC, CurrLine),
    case lists:split(ToL - L - 1, RestLines) of
        {_, []} ->
            string:split(Prefix ++ Insert, "\n") ++ RestLines;
        {_, [CurrSuffix | SuffixLines]} ->
            {_, Suffix} = lists:split(ToC, ensure_string(CurrSuffix)),
            string:split(Prefix ++ Insert ++ Suffix, "\n") ++ SuffixLines
    end.

-spec lines_to_bin(lines()) -> text().
lines_to_bin(Lines) ->
    els_utils:to_binary(lists:join("\n", Lines)).

-spec bin_to_lines(text()) -> lines().
bin_to_lines(Text) ->
    [Bin || Bin <- binary:split(Text, [<<"\r\n">>, <<"\n">>], [global])].

-spec ensure_string(binary() | string()) -> string().
ensure_string(Text) when is_binary(Text) ->
    els_utils:to_list(Text).

-spec strip_comments(binary()) -> binary().
strip_comments(Text) ->
    lines_to_bin(
        lists:map(
            fun(Line) ->
                hd(string:split(Line, "%"))
            end,
            bin_to_lines(Text)
        )
    ).

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec line_starts(text()) -> [{integer(), any()}].
line_starts(Text) ->
    [{-1, 1} | binary:matches(Text, <<"\n">>)].

-spec pos([{integer(), any()}], {line_num(), column_num()}) ->
    pos_integer().
pos(LineStarts, {LineNum, ColumnNum}) ->
    {LinePos, _} = lists:nth(LineNum, LineStarts),
    LinePos + ColumnNum.
