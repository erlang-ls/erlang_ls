-module(behaviour_c).

-callback bc_cb() ->
    term().
