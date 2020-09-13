%%=============================================================================
%% @doc Code for the Erlang DAP Agent
%%
%% This module is injected in the Erlang node that the DAP server launches.
%% @end
%%=============================================================================
-module(els_dap_agent).

-export([ int_cb/2 ]).

-spec int_cb(pid(), pid()) -> ok.
int_cb(Thread, ProviderPid) ->
  ProviderPid ! {int_cb, Thread}.
