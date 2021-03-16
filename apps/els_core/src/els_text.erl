%%==============================================================================
%% Text Manipulation Functions
%%==============================================================================
-module(els_text).

-export([ last_token/1
        , line/2
        , line/3
        , range/3
        , tokens/1
        ]).

-type text()       :: binary().
-type line_num()   :: non_neg_integer().
-type column_num() :: pos_integer().
-type token()      :: erl_scan:token().

%% @doc Extract the N-th line from a text.
-spec line(text(), line_num()) -> text().
line(Text, LineNum) ->
  Lines = binary:split(Text, <<"\n">>, [global]),
  lists:nth(LineNum + 1, Lines).

%% @doc Extract the N-th line from a text, up to the given column number.
-spec line(text(), line_num(), column_num()) -> text().
line(Text, LineNum, ColumnNum) ->
  Line = line(Text, LineNum),
  binary:part(Line, {0, ColumnNum}).

%% @doc Extract a snippet from a text, from [StartLoc..EndLoc).
-spec range(text(), {line_num(), column_num()}, {line_num(), column_num()}) ->
        text().
range(Text, StartLoc, EndLoc) ->
  LineStarts = line_starts(Text),
  StartPos = pos(LineStarts, StartLoc),
  EndPos = pos(LineStarts, EndLoc),
  binary:part(Text, StartPos, EndPos - StartPos).

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
