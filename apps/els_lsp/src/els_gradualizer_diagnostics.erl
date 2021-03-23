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
    {ok, Document} = els_utils:lookup_document(Uri),
    try gradualizer:type_check_file(
          unicode:characters_to_list(Path),
          [return_errors]) of
        Errors ->
            lists:flatmap(
              fun({_Path,Error}) ->
                      FmtError = gradualizer_fmt:format_type_error(
                                   Error
                                  , [{fmt_location, brief}, {color, never}]),
                      case re:run(
                             FmtError, "([0-9]+):([0-9]+)(.*)",
                             [{capture, all_but_first, binary}]) of
                          {match, [BinLine, BinCol, Msg]} ->
                              Line = binary_to_integer(BinLine),
                              Col = binary_to_integer(BinCol),
                              ?LOG_ERROR("Get element at pos: ~p",[{Line,Col}]),
                              Range = case
                                          els_dt_document:get_element_at_pos(Document, Line, Col)
                                      of
                                          [#{ range := R } | _] -> R;
                                          []        ->
                                              els_protocol:range(
                                                #{ from => {Line,1},
                                                   to => {Line + 1, 1} })
                                      end,
                              [#{ range => Range,
                                 message => Msg,
                                 severity => ?DIAGNOSTIC_WARNING,
                                 source => source() }];
                          _ ->
                              []
                      end
              end, Errors)
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
