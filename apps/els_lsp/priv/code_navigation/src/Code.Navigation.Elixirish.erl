-module('Code.Navigation.Elixirish').

-type 'Type'(T) :: [T, ...].

-export([ do/1 ]).
-export_type([ 'Type'/1 ]).

do(T) ->
    [T].
