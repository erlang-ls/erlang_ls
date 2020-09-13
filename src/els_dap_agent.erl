-module(els_dap_agent).

-export([ int_cb/2 ]).

-spec int_cb(pid(), pid()) -> ok.
int_cb(Thread, ProviderPid) ->
  ProviderPid ! {int_cb, Thread}.
