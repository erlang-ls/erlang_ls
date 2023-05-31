-module(els_dap_rpc).

-export([
    interpreted/1,
    n/2,
    all_breaks/1,
    all_breaks/2,
    auto_attach/3,
    break/3,
    break_in/4,
    clear/1,
    continue/2,
    delete_break/3,
    eval/3,
    file/3,
    get_meta/2,
    halt/1,
    i/2,
    interpretable/2,
    load_binary/4,
    meta/4,
    meta_eval/3,
    module_info/3,
    next/2,
    no_break/1,
    no_break/2,
    snapshot/1,
    stack_trace/2,
    step/2
]).

-include_lib("kernel/include/logger.hrl").

-spec interpreted(node()) -> any().
interpreted(Node) ->
    rpc:call(Node, int, interpreted, []).

-spec n(node(), any()) -> any().
n(Node, Module) ->
    rpc:call(Node, int, n, [Module]).

-spec all_breaks(node()) -> any().
all_breaks(Node) ->
    rpc:call(Node, int, all_breaks, []).

-spec all_breaks(node(), atom()) -> any().
all_breaks(Node, Module) ->
    rpc:call(Node, int, all_breaks, [Module]).

-spec auto_attach(node(), [atom()], {module(), atom(), [any()]}) -> any().
auto_attach(Node, Flags, MFA) ->
    rpc:call(Node, int, auto_attach, [Flags, MFA]).

-spec break(node(), module(), integer()) -> any().
break(Node, Module, Line) ->
    rpc:call(Node, int, break, [Module, Line]).

-spec break_in(node(), module(), atom(), non_neg_integer()) -> any().
break_in(Node, Module, Func, Arity) ->
    rpc:call(Node, int, break_in, [Module, Func, Arity]).

-spec clear(node()) -> ok.
clear(Node) ->
    rpc:call(Node, int, clear, []).

-spec continue(node(), pid()) -> any().
continue(Node, Pid) ->
    rpc:call(Node, int, continue, [Pid]).

-spec delete_break(node(), module(), non_neg_integer()) -> any().
delete_break(Node, Module, Line) ->
    rpc:call(Node, int, delete_break, [Module, Line]).

-spec eval(node(), string(), [any()]) -> any().
eval(Node, Input, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Input) ++ "."),
    {ok, Exprs} = erl_parse:parse_exprs(Tokens),

    case rpc:call(Node, erl_eval, exprs, [Exprs, Bindings]) of
        {value, Value, _NewBindings} -> Value;
        {badrpc, Error} -> Error
    end.

-spec file(node(), module(), binary()) -> {ok, file:filename()} | {error, not_found}.
file(Node, Module, Cwd) ->
    ?LOG_DEBUG("Looking in Int: [~p]", [Module]),
    case file_from_module_info(Node, Module, Cwd) of
        {ok, FileFromInt} ->
            {ok, FileFromInt};
        {error, not_found} ->
            case file_from_int(Node, Module) of
                {ok, FileFromModuleInfo} ->
                    {ok, FileFromModuleInfo};
                {error, not_found} ->
                    file_from_code_server(Node, Module)
            end
    end.

-spec file_from_int(node(), module()) -> {ok, file:filename()} | {error, not_found}.
file_from_int(Node, Module) ->
    ?LOG_DEBUG("Looking in Int: [~p]", [Module]),
    case rpc:call(Node, int, file, [Module]) of
        {error, not_loaded} ->
            ?LOG_DEBUG("Not Found in Int: [~p]", [Module]),
            {error, not_found};
        Path ->
            ?LOG_DEBUG("Found in Int: [~p]", [Path]),
            {ok, Path}
    end.

-spec file_from_code_server(node(), module()) -> {ok, file:filename()} | {error, not_found}.
file_from_code_server(Node, Module) ->
    ?LOG_DEBUG("Looking in Code Server: [~p]", [Module]),
    BeamName = atom_to_list(Module) ++ ".beam",
    case rpc:call(Node, code, where_is_file, [BeamName]) of
        non_existing ->
            ?LOG_DEBUG("Not found in Code Server: [~p]", [Module]),
            {error, not_found};
        BeamFile ->
            ?LOG_DEBUG("Found in Code Server: [~p]", [BeamFile]),
            rpc:call(Node, filelib, find_source, [BeamFile])
    end.

-spec file_from_module_info(node(), module(), binary()) ->
    {ok, file:filename()} | {error, not_found}.
file_from_module_info(Node, Module, Cwd) ->
    ?LOG_DEBUG("Looking in Module Info: [~p]", [Module]),
    CompileOpts = module_info(Node, Module, compile),
    case proplists:get_value(source, CompileOpts) of
        undefined ->
            ?LOG_DEBUG("Not found in Module Info: [~p]", [Module]),
            {error, not_found};
        Source ->
            ?LOG_DEBUG("Found in Module Info: [~p]", [Source]),
            case filelib:safe_relative_path(Source, Cwd) of
                unsafe ->
                    %% File is already absolute
                    regular_file(Node, Source);
                RelativePath ->
                    AbsolutePath = filename:join(Cwd, RelativePath),
                    ?LOG_DEBUG("Composed Absolute Path as: [~p]", [AbsolutePath]),
                    regular_file(Node, AbsolutePath)
            end
    end.

-spec regular_file(node(), file:filename()) -> {ok, file:filename()} | {error, not_found}.
regular_file(Node, Path) ->
    case rpc:call(Node, filelib, is_regular, [Path]) of
        true ->
            {ok, Path};
        false ->
            ?LOG_DEBUG("File is not regular: [~p]", [Path]),
            {error, not_found}
    end.

-spec get_meta(node(), pid()) -> {ok, pid()}.
get_meta(Node, Pid) ->
    rpc:call(Node, dbg_iserver, safe_call, [{get_meta, Pid}]).

-spec halt(node()) -> true.
halt(Node) ->
    rpc:cast(Node, erlang, halt, []).

-spec i(node(), module()) -> any().
i(Node, Module) ->
    rpc:call(Node, int, i, [Module]).

-spec interpretable(node(), module() | string()) ->
    true
    | {error, no_src | no_beam | no_debug_info | badarg | {app, kernel | stdlib | gs | debugger}}.
interpretable(Node, AbsModule) ->
    rpc:call(Node, int, interpretable, [AbsModule]).

-spec load_binary(node(), module(), string(), binary()) -> any().
load_binary(Node, Module, File, Bin) ->
    rpc:call(Node, code, load_binary, [Module, File, Bin]).

-spec meta(node(), pid(), atom(), any()) -> any().
meta(Node, Meta, Flag, Opt) ->
    rpc:call(Node, int, meta, [Meta, Flag, Opt]).

-spec meta_eval(node(), pid(), string()) -> any().
meta_eval(Node, Meta, Command) ->
    rpc:call(Node, els_dap_agent, meta_eval, [Meta, Command]).

-spec next(node(), pid()) -> any().
next(Node, Pid) ->
    rpc:call(Node, int, next, [Pid]).

-spec no_break(node()) -> ok.
no_break(Node) ->
    rpc:call(Node, int, no_break, []).

-spec no_break(node(), atom()) -> ok.
no_break(Node, Module) ->
    rpc:call(Node, int, no_break, [Module]).

-spec module_info(node(), module(), atom()) -> any().
module_info(Node, Module, What) ->
    rpc:call(Node, Module, module_info, [What]).

-spec snapshot(node()) -> any().
snapshot(Node) ->
    rpc:call(Node, int, snapshot, []).

-spec stack_trace(node(), any()) -> any().
stack_trace(Node, Flag) ->
    rpc:call(Node, int, stack_trace, [Flag]).

-spec step(node(), pid()) -> any().
step(Node, Pid) ->
    rpc:call(Node, int, step, [Pid]).
