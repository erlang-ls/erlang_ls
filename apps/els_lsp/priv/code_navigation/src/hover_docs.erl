-module(hover_docs).

-export([ multiple_clauses/1 ]).

multiple_clauses(L) when is_list(L) ->
  42;
multiple_clauses(#{data := Data}) ->
  Data;
multiple_clauses(X) ->
  X - 12.
