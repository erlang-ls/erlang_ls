-module(edoc_diagnostics).

-export([main/0]).

%% @mydoc Main function
main() ->
  internal().

%% @docc internal
internal() ->
  ok.

%% @doc `
unused() ->
  ok.
