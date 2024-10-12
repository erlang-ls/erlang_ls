-module(code_action_browse_docs).

-spec function_a(file:filename()) -> pid().
function_e(L) ->
    lists:sort(L),
    self().

-spec function_b() -> my_dep_mod:my_type().
function_f() ->
    my_dep_mod:my_function().
