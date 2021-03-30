-module(rename_type).
-export_type([foo/0, bar/0, foobar/0]).

-type foo() :: any().
-opaque bar() :: [rename_type:foo()].
-type foobar() :: foo() | bar().
