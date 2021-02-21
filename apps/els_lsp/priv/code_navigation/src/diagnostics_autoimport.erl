-module(diagnostics_autoimport).

-export([main/1]).

main(_Args) ->
    fun atom_to_list/1.
