-module(rename_function).
-export([foo/1, 'Quoted_Atom'/1]).

-spec foo(any()) -> any().
foo(1) ->
  1;
foo(2) ->
  2;
foo(_) ->
  anything.

bar() ->
  ok = rename_function:foo(1),
  F = fun foo/1,
  ok = F(),
  F2 = fun rename_function:foo/1,
  ok = F2(),
  F3 = fun foo/1,
  foo(F3).

-spec 'Quoted_Atom'(any()) -> any(). %% TODO: Replace Quoted_Atom -> Quoted.Atom
'Quoted_Atom'(1) ->
  1;
'Quoted_Atom'(2) ->
  2;
'Quoted_Atom'(_) ->
  anything.

baz() ->
  ok = rename_function:'Quoted_Atom'(1),
  F = fun 'Quoted_Atom'/1,
  ok = F(),
  F2 = fun rename_function:'Quoted_Atom'/1,
  ok = F2(),
  'Quoted_Atom'(F2).
