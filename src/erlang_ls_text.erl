%%==============================================================================
%% Text Manipulation Functions
%%==============================================================================
-module(erlang_ls_text).

-export([ last_token/1
        , line/2
        , line/3
        ]).

-type text()       :: binary().
-type line_num()   :: non_neg_integer().
-type column_num() :: non_neg_integer().
-type token()      :: erl_scan:token().

%% Extract the N-th line from a text.
-spec line(text(), line_num()) -> text().
line(Text, LineNum) ->
  Lines = binary:split(Text, <<"\n">>, [global]),
  lists:nth(LineNum + 1, Lines).

%% Extract the N-th line from a text, up to the given column number.
-spec line(text(), line_num(), column_num()) -> text().
line(Text, LineNum, ColumnNum) ->
  Line = line(Text, LineNum),
  binary:part(Line, {0, ColumnNum}).

%% Extract the last token from the given text.
-spec last_token(text()) -> token().
last_token(Text) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Text)),
  [Token | _]     = lists:reverse(Tokens),
  Token.
