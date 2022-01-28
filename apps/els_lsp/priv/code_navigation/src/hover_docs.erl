-module(hover_docs).

-export([ multiple_clauses/1, edoc/0 ]).

multiple_clauses(L) when is_list(L) ->
  42;
multiple_clauses(#{data := Data}) ->
  Data;
multiple_clauses(X) ->
  X - 12.

%% @doc An edoc hover item
-spec edoc() -> ok.
edoc() -> ok.