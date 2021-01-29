-module(els_dap_rpc).

-export([ all_breaks/1
        , auto_attach/3
        , break/3
        , break_in/4
        , continue/2
        , eval/3
        , get_meta/2
        , i/2
        , load_binary/4
        , meta/4
        , module_info/3
        , next/2
        , no_break/1
        , snapshot/1
        , stack_trace/2
        , step/2
        ]).


-spec all_breaks(node()) -> any().
all_breaks(Node) ->
  rpc:call(Node, int, all_breaks, []).

-spec auto_attach(node(), [atom()], {module(), atom(), [any()]}) -> any().
auto_attach(Node, Flags, MFA) ->
  rpc:call(Node, int, auto_attach, [Flags, MFA]).

-spec break(node(),  module(), integer()) -> any().
break(Node, Module, Line) ->
  rpc:call(Node, int, break, [Module, Line]).

-spec break_in(node(), module(), atom(), non_neg_integer()) -> any().
break_in(Node, Module, Func, Arity) ->
  rpc:call(Node, int, break_in, [ Module, Func, Arity]).

-spec continue(node(), pid()) -> any().
continue(Node, Pid) ->
  rpc:call(Node, int, continue, [Pid]).

-spec eval(node(), string(), [any()]) -> any().
eval(Node, Input, Bindings) ->
  {ok, Tokens, _} = erl_scan:string(unicode:characters_to_list(Input) ++ "."),
  {ok, Exprs} = erl_parse:parse_exprs(Tokens),

  case rpc:call(Node, erl_eval, exprs, [Exprs, Bindings]) of
    {value, Value, _NewBindings} -> Value;
    {badrpc, Error} -> Error
  end.

-spec get_meta(node(), pid()) -> {ok, pid()}.
get_meta(Node, Pid) ->
  rpc:call(Node, dbg_iserver, safe_call, [{get_meta, Pid}]).

-spec i(node(), module()) -> any().
i(Node, Module) ->
  rpc:call(Node, int, i, [Module]).

-spec load_binary(node(), module(), string(), binary()) -> any().
load_binary(Node, Module, File, Bin) ->
  rpc:call(Node, code, load_binary, [Module, File, Bin]).

-spec meta(node(), pid(), atom(), atom()) -> any().
meta(Node, Meta, Flag, Opt) ->
  rpc:call(Node, int, meta, [Meta, Flag, Opt]).

-spec next(node(), pid()) -> any().
next(Node, Pid) ->
  rpc:call(Node, int, next, [Pid]).

-spec no_break(node()) -> ok.
no_break(Node) ->
  rpc:call(Node, int, no_break, []).

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
