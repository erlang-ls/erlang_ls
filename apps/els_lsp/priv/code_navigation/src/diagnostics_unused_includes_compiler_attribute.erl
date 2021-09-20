%% Issue 1047
-module(diagnostics_unused_includes_compiler_attribute).

-include_lib("kernel/include/file.hrl").
-include_lib("eunit/include/eunit.hrl").

the_test() ->
  ok.
