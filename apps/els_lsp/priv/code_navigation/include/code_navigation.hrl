-record(included_record_a, {included_field_a, included_field_b}).

-define(INCLUDED_MACRO_A, included_macro_a).

-type included_type_a() :: ok.

-record('PascalCaseRecord', {'Field #1', 'Field #2'}).

-type 'INCLUDED_TYPE'(T) :: T | undefined.
