-module(hover_docs_caller).

-export([ local_call_no_args/0
        , local_call_with_args/0
        , remote_call_multiple_clauses/0
        ]).

local_call_no_args() ->
  local_call().

local_call_with_args() ->
  local_call(dummy_arg1, dummy_arg2).

remote_call_multiple_clauses() ->
  hover_docs:multiple_clauses(dummy_arg).

local_call() ->
  ok.

-spec local_call(integer(), any()) -> tuple();
                (float(), any()) -> tuple().
local_call(Arg1, Arg2) ->
  {Arg1, Arg2}.
