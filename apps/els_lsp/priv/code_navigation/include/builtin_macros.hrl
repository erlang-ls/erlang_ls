-export([f/0]).

-spec f() -> any().
f() ->
    ?MODULE,
    ?MODULE_STRING,
    ?FILE,
    ?LINE,
    ?MACHINE,
    ?FUNCTION_NAME,
    ?FUNCTION_ARITY,
    ?OTP_RELEASE.
