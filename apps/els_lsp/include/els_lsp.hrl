-ifndef(__ELS_LSP_HRL__).
-define(__ELS_LSP_HRL__, 1).

-include_lib("els_core/include/els_core.hrl").

-define(APP, els_lsp).

-define(ELP_LOG_FORMAT, [ "[", level, "] ", "[", time, "] ", mfa, " L", line, " ", pid,  "\n", msg, "\n" ]).

-endif.
