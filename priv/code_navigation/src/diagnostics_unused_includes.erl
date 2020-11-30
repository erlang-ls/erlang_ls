-module(diagnostics_unused_includes).

-include_lib("kernel/include/file.hrl").
-include_lib("et/include/et.hrl").
-include("code_navigation.hrl").
-include("diagnostics.hrl").
-include("transitive.hrl").

-export([main/0]).

-spec main() -> {defined_type(), any()}.
main() ->
  _ = #file_info{},
  {?INCLUDED_MACRO_A, ?MACRO_FOR_TRANSITIVE_INCLUSION}.
