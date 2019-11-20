-module(code_navigation_extra).

-export([ do/1, do_2/0 ]).

do(_Config) ->
  ok.

do_2() ->
  code_navigation:function_h().

-spec do_3(nat(), wot()) -> {atom(), code_navigation_types:type_a()}.
do_3(_, _) ->
  code_navigation:function_j().

-spec do_4(nat(), opaque_local()) -> {atom(), code_navigation_types:opaque_type_a()}.
do_4(_, _) ->
  code_navigation:function_j().

-opaque opaque_local() :: atom().
