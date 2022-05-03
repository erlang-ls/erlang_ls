-module(completion_clause).

-export([f/1]).

f(Arg1) ->
    case Arg1 of
        42 -> fun (1) -> one;
                  (2) -> two
              end;
        _ -> ok
    end;
f(_) ->
    ok.
