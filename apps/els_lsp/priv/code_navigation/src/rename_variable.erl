-module(rename_variable).
-callback name(Var) -> Var.
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

-record(foo, {a :: Var,
              b :: [Var]}).

-define(MACRO(Var), Var + Var).

-type type(Var) :: Var.
-opaque opaque(Var) :: Var.

foo(Var) ->
  Var.

-if(Var == Var).

-endif.
