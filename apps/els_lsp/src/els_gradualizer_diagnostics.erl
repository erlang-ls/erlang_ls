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
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  Files = lists:flatmap(
            fun(Dir) ->
                filelib:wildcard(filename:join(Dir, "*.erl"))
            end,
            els_config:get(apps_paths) ++ els_config:get(deps_paths)),
  ?LOG_INFO("Importing files into gradualizer_db: ~p", [Files]),
  ok = gradualizer_db:import_erl_files(Files,
                                       els_config:get(include_paths)),
  Path = unicode:characters_to_list(els_uri:path(Uri)),
  Includes = [{i, I} || I <- els_config:get(include_paths)],
  Opts = [return_errors] ++ Includes,
  Errors = gradualizer:type_check_files([Path], Opts),
  ?LOG_INFO("Gradualizer diagnostics: ~p", [Errors]),
  lists:flatmap(fun analyzer_error/1, Errors).

-spec source() -> binary().
source() ->
  <<"Gradualizer">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec analyzer_error(any()) -> any().
analyzer_error({_Path, Error}) ->
  FmtOpts = [{fmt_location, brief}, {color, never}],
  FmtError = gradualizer_fmt:format_type_error(Error, FmtOpts),
  case re:run(FmtError, "([0-9]+):([0-9]+:)? (.*)",
              [{capture, all_but_first, binary}, dotall]) of
    {match, [BinLine, _BinCol, Msg]} ->
      Line = case binary_to_integer(BinLine) of
               0 -> 1;
               L -> L
             end,
      Range = els_protocol:range(#{ from => {Line, 1},
                                    to => {Line + 1, 1} }),
      [els_diagnostics:make_diagnostic(Range, Msg, ?DIAGNOSTIC_WARNING,
                                       source())];
    _ ->
      []
  end.
