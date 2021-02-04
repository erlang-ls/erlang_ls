
%% blah blah
-include("nonexisten-file.hrl").

-if(?MODULE == module_foo).  % TODO get rid of
-else.
-error("including nonexistent_macro.hrl is not allowed").
-endif.
