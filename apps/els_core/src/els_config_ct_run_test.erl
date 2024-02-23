-module(els_config_ct_run_test).

%% We may introduce a behaviour for config modules in the future
-export([default_config/0]).

%% Getters
-export([
    get_module/0,
    get_function/0
]).

-type config() :: #{string() => string()}.

-spec default_config() -> config().
default_config() ->
    #{
        "module" => default_module(),
        "function" => default_function()
    }.

-spec get_module() -> atom().
get_module() ->
    Value = maps:get("module", els_config:get('ct-run-test'), default_module()),
    list_to_atom(Value).

-spec get_function() -> atom().
get_function() ->
    Value = maps:get(
        "function",
        els_config:get('ct-run-test'),
        default_function()
    ),
    list_to_atom(Value).

-spec default_module() -> string().
default_module() ->
    "rebar3_erlang_ls_agent".

-spec default_function() -> string().
default_function() ->
    "run_ct_test".
