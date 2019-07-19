-module(erlang_ls).

-export([ main/1 ]).

-spec main([any()]) -> ok.
main([Port]) ->
  application:set_env(erlang_ls, port, list_to_integer(Port)),
  application:ensure_all_started(erlang_ls),
  receive _ -> ok end.
