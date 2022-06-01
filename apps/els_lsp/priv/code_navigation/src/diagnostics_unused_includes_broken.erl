-module(diagnostics_unused_includes_broken).

-include_lib(
    "foo"-include_lib("foo")
).
