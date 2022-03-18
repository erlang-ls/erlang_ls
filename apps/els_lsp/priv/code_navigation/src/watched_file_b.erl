-module(watched_file_b).

-export([ main/0 ]).

main() ->
  watched_file_a:main().
