-module(els_doctor).

-export([ nodes/0
        , config/1
        ]).

-spec nodes() -> [string()].
nodes() ->
  {ok, Names} = erl_epmd:names(),
  [list_to_atom(Name) ||
    {Name, _Port} <- Names, re:run(Name, "erlang_ls_*") =/= nomatch].

-spec config(string()) -> ok.
config(Name) ->
  rpc:call(els_utils:compose_node_name(atom_to_list(Name), shortnames)
          , els_config, get, [apps_paths]).
