-module(completion_resolve).

-export([ call_1/0
        , main/0
        ]).

%% @doc Call me maybe
-spec call_1() -> 'ok'.
call_1() ->
  ok.

main() ->
  completion_resolve:call_1(),
  call_1(),
  completion_resolve_2:call_1(),
  file:write(a, b),
  ok.

%% @doc Call me sometime
-spec call_2() -> 'ok'.
call_2() ->
  ok.