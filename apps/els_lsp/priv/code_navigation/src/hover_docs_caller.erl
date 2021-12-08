-module(hover_docs_caller).

-export([ local_call_no_args/0
        , local_call_with_args/0
        , remote_call_multiple_clauses/0
        , implicit_funs/0
        ]).

local_call_no_args() ->
  local_call().

local_call_with_args() ->
  local_call(dummy_arg1, dummy_arg2).

remote_call_multiple_clauses() ->
  hover_docs:multiple_clauses(dummy_arg).

implicit_funs() ->
  {fun local_call/2,
   fun hover_docs:multiple_clauses/1}.

remote_call_edoc() ->
  hover_docs:edoc().

remote_call_otp() ->
  file:write(a, b).

local_call_edoc() ->
  edoc().

local_call() ->
  ok.

-spec local_call(integer(), any()) -> tuple();
                (float(), any()) -> tuple().
local_call(Arg1, Arg2) ->
  {Arg1, Arg2}.

%% @doc An edoc hover item
-spec edoc() -> ok.
edoc() -> ok.