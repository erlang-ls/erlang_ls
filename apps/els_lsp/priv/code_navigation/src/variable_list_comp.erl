-module(variable_list_comp).

one() ->
    Var = 1,
    [ Var || Var <- [1, 2, 3] ],
    Var.

two() ->
    [ Var || Var <- [1, 2, 3] ],
    [ Var || Var <- [4, 5, 6] ].

three() ->
    Var = 1,
    [ Var || _ <- [1, 2, 3] ],
    Var.

four() ->
    [ {Var, Var2} || Var <- [4, 5, 6],
                     Var2 <- [ Var || Var <- [1, 2, 3] ]
    ].
