-module(code_navigation_types).

-type type_a() :: atom().

-export_type([ type_a/0, type_b/0, user_type_c/0 ]).

-opaque opaque_type_a() :: atom().

-export_type([ opaque_type_a/0 ]).

-type user_type_a() :: type_a() | opaque_type_a().

-include("transitive.hrl").

-type user_type_b() :: type_b().
-type user_type_c() :: user_type_b().
-record(record_a,
       {field_a = an_atom :: user_type_a()}).

-type user_type_c() :: #{
                         key_a := user_type_a()
                        }.

-spec function_a(A :: user_type_a()) -> B :: user_type_b().
function_a(type_a) ->
    type_b.

-type user_type_d() :: type() | code_navigation:().
