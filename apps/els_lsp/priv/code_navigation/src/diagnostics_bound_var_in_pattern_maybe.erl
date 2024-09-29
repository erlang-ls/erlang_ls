%% https://github.com/erlang-ls/erlang_ls/issues/1527
%% Only valid for OTP25 or higher
-module(diagnostics_bound_var_in_pattern_maybe).

-if(?OTP_RELEASE<27).
-feature(maybe_expr, enable).
-endif.

-export([foo/0, maybe_expr/0, no_else/0]).

foo() ->
    maybe
        X ?= bar(),
        X == foo
    else
        e = Error -> Error
          % ^- Bound variable in pattern: Error
    end.

bar() ->
    foo.

maybe_expr() ->
    X = 1,
    Y = ok,
    maybe
        X ?= 1
    else
        Y -> Y
    end.

no_else() ->
    Y = 1,
    maybe
        {ok, Y} ?= 2,
        X = 4,
        Z ?= 8,
        X == Z
    end.
