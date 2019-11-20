-module(code_navigation_types).

-type type_a() :: atom().

-export_type([ type_a/0 ]).

-opaque opaque_type_a() :: atom().

-export_type([ opaque_type_a/0 ]).
