-module(purge_references).

-spec foo(any()) -> ok.
foo(_X) -> ok.
bar() -> foo(42).
