-module(erlang_ls).

-export([ main/1 ]).

-spec main([any()]) -> ok.
main([Port]) ->
  init_node_name(is_debug()),
  application:set_env(erlang_ls, port, list_to_integer(Port)),
  application:ensure_all_started(erlang_ls),
  lager:info("Started erlang_ls server ~p", [node()]),
  receive _ -> ok end.

-spec init_node_name(boolean()) -> ok.
init_node_name(true) ->
  ok       = erlang_ls_utils:start_epmd(),
  Name     = "erlang_ls_" ++ integer_to_list(rand:uniform(16#FFFFFFFFF)),
  NodeName = list_to_atom(Name),
  net_kernel:start([NodeName, shortnames]),
  ok;
init_node_name(false) ->
  ok.

-spec is_debug() -> boolean().
is_debug() ->
  application:load(erlang_ls),
  application:get_env(erlang_ls, debug_mode, false).
