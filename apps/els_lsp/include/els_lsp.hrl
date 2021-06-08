-ifndef(__ELS_LSP_HRL__).
-define(__ELS_LSP_HRL__, 1).

-include_lib("els_core/include/els_core.hrl").

-define(APP, els_lsp).

-define(LSP_LOG_FORMAT, ["[", time, "] ", "[", level, "] ", msg, " [", mfa,  " L", line, "] ", pid, "\n"]).

-endif.
