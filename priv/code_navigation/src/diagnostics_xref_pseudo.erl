-module(diagnostics_xref_pseudo).

-export([ main/0 ]).

-record(person, {name, phone, address}).

main() ->
  record_info(fields, person),
  module_info(),
  module_info(module),
  ?MODULE:behaviour_info(callbacks),
  ok.
