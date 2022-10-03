-module(implementation).
-export([call/1]).
-callback to_be_implemented() -> ok.

call(Mod) ->
    Mod:to_be_implemented().
