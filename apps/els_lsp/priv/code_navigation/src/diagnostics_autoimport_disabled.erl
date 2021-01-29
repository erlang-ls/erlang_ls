-module(diagnostics_autoimport_disabled).
-compile(no_auto_import).

-export([main/1]).

main(_Args) ->
    fun atom_to_list/1.
