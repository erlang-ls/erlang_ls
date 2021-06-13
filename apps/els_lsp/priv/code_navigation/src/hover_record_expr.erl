-module(hover_record_expr).
-include("hover_record.hrl").

-record(test_record, {
        field1 = 123,
        field2 = xyzzy,
        field3
}).

f(Record) ->
  #test_record{field1 = Field1} = Record,
  Field1.

g(Record) ->
  #included_record_a{included_field_b = FieldB} = Record,
  FieldB.
