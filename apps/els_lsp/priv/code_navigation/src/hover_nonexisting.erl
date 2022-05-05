-module(hover_nonexisting).

-export([main/0]).

main() ->
  nonexisting:main().
