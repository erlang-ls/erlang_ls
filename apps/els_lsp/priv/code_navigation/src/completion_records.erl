-module(completion_records).

-record(record_a, {field_a, field_b, 'Field C'}).
-record(record_b, {field_x, field_y}).

function_a(#record_a{field_a = a, field_b = b}) ->
    #record_b{field_x = #record_a{},
              %% #record_a{
              field_y = y},
    {}.
