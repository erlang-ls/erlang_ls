-module(diagnostics_bound_var_in_pattern_cannot_parse).

f(Var1) ->
  Var1 = 1.

g() ->'
