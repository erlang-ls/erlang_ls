-module(hover_type).

-type type_a() :: any().
-opaque opaque_type_a() :: any().

-spec f(type_a()) -> ok.
f(_) ->
  ok.

-spec g(hover_type_remote:type_a()) -> ok.
g(_) ->
  ok.

-spec h(opaque_type_a()) -> ok.
h(_) ->
  ok.

-spec i(hover_type_remote:opaque_type_a()) -> ok.
i(_) ->
  ok.

-spec j(doesnt:exist()) -> ok.
j(_) ->
  ok.
