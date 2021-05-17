-module(code_navigation_types).

-type type_a() :: atom().

-export_type([ type_a/0 ]).

-opaque opaque_type_a() :: atom().

-export_type([ opaque_type_a/0 ]).

-type user_type_a() :: type_a() | opaque_type_a().

-include("transitive.hrl").

-type user_type_b() :: type_b().
