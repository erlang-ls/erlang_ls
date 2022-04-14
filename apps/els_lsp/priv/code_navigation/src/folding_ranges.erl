-module(folding_ranges).

function_foldable() ->
  ?assertEqual(2.0, 4/2).

-record(unfoldable_record, { field_a }).

-record(foldable_record, { field_a
                         , field_b
                         , field_c
                        }).
