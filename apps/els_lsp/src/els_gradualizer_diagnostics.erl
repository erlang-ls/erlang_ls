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
            FmtErrors = [gradualizer_fmt:format_type_error(Error,[{fmt_location,brief}])
             || {_Path,Error} <- Errors],
            ?LOG_ERROR("FmtErrors: ~p",[FmtErrors]),
            lists:flatten(
              [ case re:run(
                     Str, "([0-9]+):([0-9]+)(.*)",
                     [{capture,all_but_first,binary}]) of
                  {match, [Line,Col,Msg]} ->
                      #{ range => els_protocol:range(
                                    #{ from => {binary_to_integer(Line),
                                                binary_to_integer(Col)},
                                       to => {binary_to_integer(Line) + 1, 1} }),
                         message => Msg,
                         severity => ?DIAGNOSTIC_WARNING,
                         source => source() };
                  _ ->
                      []
              end || Str <- FmtErrors])
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
