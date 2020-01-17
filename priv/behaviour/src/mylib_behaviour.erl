-module(mylib_behaviour).

-callback init(any())                -> any().
-callback foo(any(), binary())       -> ok.
