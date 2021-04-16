-module(diagnostics_bound_var_in_pattern).

-export([f/1, g/1, h/2, fun_expr/1, named_fun_expr/0]).

f(Var1) ->
  Var1 = 1.

g(Var2) ->
  case a:b() of
    {ok, Var2} -> ok;
    _ -> error
  end.

h(Var3, Var4) ->
  try a:b() of
    {New, Var3} ->
      New
  catch Var4 ->
      error
  end.

fun_expr(New) ->
  fun(New, Var5) ->
      Var5 = New
  end.

named_fun_expr() ->
  fun F(New, Var6) ->
      New = Var6,
      F = Var6
  end.
