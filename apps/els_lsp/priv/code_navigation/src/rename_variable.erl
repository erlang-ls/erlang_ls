-module(rename_variable).

foo(Var) ->
  Var < 0;
foo(Var) ->
  Var2 = Var,
  Var > 0 andalso Var =:= Var2;
foo(_Var) ->
  false.

bar(Var) ->
  Var.

-spec baz(Var) -> Var
          when Var :: atom().
baz(Var) ->
  Var.
