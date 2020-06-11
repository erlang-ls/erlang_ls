-module(diagnostics_compiler_with_include_of_third_party_dep_header).

-include_lib("some_dep/include/some_dep.hrl").

-export([ main/1 ]).

main(_Args) ->
  #some_dep_record{
    field1 = 1,
    field2 = 2
  }.
