-module(completion_resolve).

-export([ call_1/0
        , call_2/0
        , main/0
        ]).

-spec call_1() -> 'ok'.
call_1() ->
  ok.

call_2() ->
  ok.

main() ->
  completion_resolve:call_1(),
  call_1(),
  ok.
