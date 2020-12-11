-module(completion_caller).

-export([ main/0 ]).

main() ->
  {fun completion:complete_1, fun completion:complete_2}.
