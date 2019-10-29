-module(erlang_ls).

-export([ main/1 ]).

-spec main([any()]) -> ok.
main([Port]) ->
  init_node_name(),
  application:set_env(erlang_ls, port, list_to_integer(Port)),
  application:ensure_all_started(erlang_ls),
  lager:info("Started erlang_ls server ~p", [node()]),
  receive _ -> ok end.

-spec init_node_name() -> ok.
init_node_name() ->
  Name     = "erlang_ls_" ++ integer_to_list(rand:uniform(16#FFFFFFFFF)),
  NodeName = list_to_atom(Name),
  net_kernel:start([NodeName, shortnames]),
  ok.
