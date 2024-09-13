%% https://github.com/erlang-ls/erlang_ls/issues/1527
%% Only valid for OTP25 or higher
-module(diagnostics_bound_var_in_pattern_maybe).

-if(?OTP_RELEASE<27).
-feature(maybe_expr, enable).
-endif.

-export([foo/0]).

foo() ->
    maybe
        foo ?= bar()
    else
        e = Error -> Error
          % ^- Bound variable in pattern: Error
    end.

bar() ->
    foo.
