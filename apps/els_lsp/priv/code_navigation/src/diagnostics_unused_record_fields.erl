-module(diagnostics_unused_record_fields).

-export([main/1]).

-record(used_field, {field_a, field_b = 42}).
-record(unused_field, {field_c, field_d}).

main(#used_field{field_a = A, field_b = B}) ->
  {A, B};
main(R) ->
  R#unused_field.field_c.
