-module(atom_typo).
-export([f/0]).

f() ->
  %% typos
  ture,
  falsee,
  fales,
  undifened,
  udefined,
  errorr,
  %% ok
  true,
  false,
  fails,
  undefined,
  unified,
  error,
  ok.
