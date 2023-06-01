%%=============================================================================
%% @doc Code for the Erlang DAP Agent
%%
%% This module is injected in the Erlang node that the DAP server launches.
%% @end
%%=============================================================================
-module(els_dap_agent).

-export([int_cb/2, meta_eval/2]).

-spec int_cb(pid(), pid()) -> ok.
int_cb(Thread, ProviderPid) ->
    case lists:keyfind(Thread, 1, int:snapshot()) of
        {_Pid, _Function, break, _Info} ->
            ProviderPid ! {int_cb, Thread};
        {_Pid, _Function, 'exit', _Info} ->
            ProviderPid ! {int_cb_exit, Thread};
        _ ->
            ok
    end,
    ok.

-spec meta_eval(pid(), string()) -> any().
meta_eval(Meta, Command) ->
    _ = int:meta(Meta, eval, {ignored_module, Command}),
    receive
        {Meta, {eval_rsp, Return}} ->
            Return
    end.
