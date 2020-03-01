%%==============================================================================
%% Text Manipulation Functions
%%==============================================================================
-module(els_text).

-export([ last_token/1
        , line/2
        , line/3
        , tokens/1
        ]).

-type text()       :: binary().
-type line_num()   :: non_neg_integer().
-type column_num() :: non_neg_integer().
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

%% @doc Return tokens from text.
-spec tokens(text()) -> [any()].
tokens(Text) ->
  case erl_scan:string(binary_to_list(Text)) of
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
