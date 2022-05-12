-module(els_config_runtime).

-include("els_core.hrl").

%% We may introduce a behaviour for config modules in the future
-export([default_config/0]).

%% Getters
-export([
    get_node_name/0,
    get_otp_path/0,
    get_start_cmd/0,
    get_start_args/0,
    get_name_type/0,
    get_cookie/0
]).

-type config() :: #{string() => string()}.

-spec default_config() -> config().
default_config() ->
    #{
        "node_name" => default_node_name(),
        "otp_path" => default_otp_path(),
        "start_cmd" => default_start_cmd(),
        "start_args" => default_start_args()
    }.

-spec get_node_name() -> atom().
get_node_name() ->
    Value = maps:get("node_name", els_config:get(runtime), default_node_name()),
    els_utils:compose_node_name(Value, get_name_type()).

-spec get_otp_path() -> string().
get_otp_path() ->
    maps:get("otp_path", els_config:get(runtime), default_otp_path()).

-spec get_start_cmd() -> string().
get_start_cmd() ->
    maps:get("start_cmd", els_config:get(runtime), default_start_cmd()).

-spec get_start_args() -> [string()].
get_start_args() ->
    Value = maps:get("start_args", els_config:get(runtime), default_start_args()),
    string:tokens(Value, " ").

-spec get_name_type() -> shortnames | longnames.
get_name_type() ->
    case maps:get("use_long_names", els_config:get(runtime), false) of
        false ->
            shortnames;
        true ->
            longnames
    end.

-spec get_cookie() -> atom().
get_cookie() ->
    case maps:get("cookie", els_config:get(runtime), undefined) of
        undefined ->
            erlang:get_cookie();
        Cookie ->
            list_to_atom(Cookie)
    end.

-spec default_node_name() -> string().
default_node_name() ->
    RootUri = els_config:get(root_uri),
    {ok, Hostname} = inet:gethostname(),
    NodeName = els_distribution_server:normalize_node_name(
        filename:basename(els_uri:path(RootUri))
    ),
    NodeName ++ "@" ++ Hostname.

-spec default_otp_path() -> string().
default_otp_path() ->
    filename:dirname(filename:dirname(code:root_dir())).

-spec default_start_cmd() -> string().
default_start_cmd() ->
    "rebar3".

-spec default_start_args() -> string().
default_start_args() ->
    "erlang_ls".
