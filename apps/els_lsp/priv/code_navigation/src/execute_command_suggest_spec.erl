-module(execute_command_suggest_spec).

-export([with_spec/2, without_spec/2]).

-type type_a() :: number().
-type type_b() :: binary().

-spec with_spec(type_a(), type_b()) -> {type_a(), type_b()}.
with_spec(A, B) ->
  {A, B}.

without_spec(A, B) when is_binary(B) ->
  {A + 42, B}.
