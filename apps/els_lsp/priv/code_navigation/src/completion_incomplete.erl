-module(completion_incomplete).

-export([ function_exported/0
        , function_
        ]).

function_exported() ->
  ok.

function_unexported() ->
  ok.
