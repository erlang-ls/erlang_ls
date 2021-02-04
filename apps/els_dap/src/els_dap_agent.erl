%%=============================================================================
%% @doc Code for the Erlang DAP Agent
%%
%% This module is injected in the Erlang node that the DAP server launches.
%% @end
%%=============================================================================
-module(els_dap_agent).

-export([ int_cb/2, meta_eval/2 ]).

-spec int_cb(pid(), pid()) -> ok.
int_cb(Thread, ProviderPid) ->
  ProviderPid ! {int_cb, Thread},
  ok.

-spec meta_eval(pid(), string()) -> any().
meta_eval(Meta, Command) ->
  _ = int:meta(Meta, eval, {ignored_module, Command}),
  receive
     {Meta, {eval_rsp, Return}} ->
       Return
  end.
