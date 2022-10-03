-module(code_navigation_extra).

-export([ do/1, do_2/0, 'DO_LOUDER'/0, function_a/2 ]).

do(_Config) ->
  do_4(1, foo).

do_2() ->
  code_navigation:function_h().

-spec do_3(nat(), wot()) -> {atom(), code_navigation_types:type_a()}.
do_3(_, _) ->
  code_navigation:function_j().

%% @doc do_4 is a local-only function
-spec do_4(nat(), opaque_local()) -> {atom(), code_navigation_types:opaque_type_a()}.
do_4(_, _) ->
  code_navigation:function_j().

-opaque opaque_local() :: atom().

'DO_LOUDER'() ->
  'Code.Navigation.Elixirish':do('Atom').

function_a(Arg1, Arg2) ->
  funct(),
  code_navigation:().
  code_navigation:funct().
