-module(diagnostics_xref).

-export([ main/0 ]).

main() ->
  lists:map(1, 2, 3),
  non_existing() ++ existing() ++ dynamic_call(foo, bar).

existing() ->
  lists:seq(1, 3).

dynamic_call(Foo, Bar) ->
  Foo:bar(),
  foo:Bar().
