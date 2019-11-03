-module(erlang_ls).

-export([ main/1 ]).

-spec main([any()]) -> ok.
main(Args) ->
  application:load(erlang_ls),
  parse_args(Args),
  init_node_name(is_debug()),
  application:ensure_all_started(erlang_ls),
  lager:info("Started erlang_ls server ~p", [node()]),
  receive _ -> ok end.

-spec parse_args([string()]) -> ok.
parse_args([]) ->
  ok;
parse_args(["--transport", Name | Rest]) ->
  Transport = case Name of
                "tcp"   -> erlang_ls_tcp;
                "stdio" -> erlang_ls_stdio
              end,
  application:set_env(erlang_ls, transport, Transport),
  parse_args(Rest);
parse_args(["--port", Port | Rest]) ->
  application:set_env(erlang_ls, port, list_to_integer(Port)),
  parse_args(Rest);
%% For backward compatibility with clients
parse_args([Port | Rest]) ->
  application:set_env(erlang_ls, port, list_to_integer(Port)),
  parse_args(Rest).

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
  {ok, DebugMode} = application:get_env(erlang_ls, debug_mode),
  DebugMode.
