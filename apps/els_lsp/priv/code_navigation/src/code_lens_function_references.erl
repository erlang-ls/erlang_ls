-module(code_lens_function_references).

-export([ a/0 ]).

-spec a() -> ok.
a() ->
  b(),
  c().

-spec b() -> ok.
b() ->
  c().

-spec c() -> ok.
c() ->
  ok.
