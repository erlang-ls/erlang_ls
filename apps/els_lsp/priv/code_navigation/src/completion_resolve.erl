-module(completion_resolve).

-export([ call_1/0
        , call_2/0
        , main/0
        ]).

%% @doc Call me maybe
-spec call_1() -> 'ok'.
call_1() ->
  ok.

call_2() ->
  ok.

main() ->
  completion_resolve:call_1(),
  call_1(),
  completion_resolve_2:call_1(),
  ok.
