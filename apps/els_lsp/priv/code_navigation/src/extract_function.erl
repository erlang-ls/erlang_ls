-module(extract_function).
-export([f/2]).

f(A, B) ->
    C = 1,
    F = A + B + C,
    G = case A of
            1 -> one;
            _ -> other
        end,
    H = [X || X <- [A, B, C], X > 1],
    I = {A, B, A},
    other_function(),
    [X || X <- [A, B, C], X > 1].

other_function() ->
    hello.
