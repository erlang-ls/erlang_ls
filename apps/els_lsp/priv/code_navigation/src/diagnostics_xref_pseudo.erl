-module(diagnostics_xref_pseudo).

-export([ main/0 ]).

-record(person, {name, phone, address}).

main() ->
  record_info(fields, person),
  module_info(),
  module_info(module),
  diagnostics_xref_pseudo:module_info(),
  diagnostics_xref_pseudo:module_info(module),
  unknown_module:module_info(),
  unknown_module:module_info(module),
  ?MODULE:behaviour_info(callbacks),
  lager:debug("log message", []),
  lager:info("log message", []),
  lager:notice("log message", []),
  lager:warning("log message", []),
  lager:error("log message", []),
  lager:critical("log message", []),
  lager:alert("log message", []),
  lager:emergency("log message", []),

  lager:debug("log message"),
  lager:info("log message"),
  lager:notice("log message"),
  lager:warning("log message"),
  lager:error("log message"),
  lager:critical("log message"),
  lager:alert("log message"),
  lager:emergency("log message"),

  % At lease one failure so we know the diagnostic is running
  unknown_module:nonexistent(),
  ok.
