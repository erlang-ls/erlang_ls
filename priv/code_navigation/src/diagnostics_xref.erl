-module(diagnostics_xref).

-export([ main/0 ]).

main() ->
  lists:map(1, 2, 3),
  non_existing() ++ existing().

existing() ->
  lists:seq(1, 3).
