%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_gradualizer_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
%% -type macro_config() :: #{string() => string()}.
%% -type macro_option() :: {atom()} | {atom(), any()}.

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
    Path = els_uri:path(Uri),
    ?LOG_ERROR("Path: ~p",[Path]),
    try gradualizer:type_check_file(
          unicode:characters_to_list(Path),
          [return_errors]) of
        Errors ->
            ?LOG_ERROR("Errors: ~p",[Errors]),
            [#{ range => els_protocol:range(#{ from => {5, 1}, to => {6, 1} }),
                message => <<"error">>,
                severity => ?DIAGNOSTIC_WARNING,
                source => source() }]
    catch E:R:ST ->
            ?LOG_ERROR("Errors: ~p",[{E,R,ST}]),
            []
    end.

-spec source() -> binary().
source() ->
  <<"Gradualizer">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================
