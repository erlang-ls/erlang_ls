-module(elvis_diagnostics).

-export([foo/2]).

%% Trigger some elvis warnings
-spec foo(any(),any()) -> any().
foo(_X,_Y) -> 3 .
