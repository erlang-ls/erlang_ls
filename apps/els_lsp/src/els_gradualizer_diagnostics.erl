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

    try application:ensure_all_started(gradualizer) of
        {ok, _} ->
            Path = unicode:characters_to_list(els_uri:path(Uri)),
            load_dependencies(),
            Errors = gradualizer:type_check_files([Path], [return_errors]),
            lists:flatmap(
              fun({ErrorPath, Error}) when Path =:= ErrorPath ->
                      FmtError = gradualizer_fmt:format_type_error(
                                   Error
                                  , [{fmt_location, brief}, {color, never}]),
                      case re:run(
                             FmtError, "([0-9]+):([0-9]+:)? (.*)",
                             [{capture, all_but_first, binary}]) of
                          {match, [BinLine, _BinCol, Msg]} ->
                              Line = binary_to_integer(BinLine),
                              Range = els_protocol:range(
                                        #{ from => {Line, 1},
                                           to => {Line + 1, 1} }),
                              [#{ range => Range,
                                 message => Msg,
                                 severity => ?DIAGNOSTIC_WARNING,
                                 source => source() }];
                          _ ->
                              []
                      end
              end, Errors)
    catch E:R:ST ->
            ?LOG_ERROR("Errors: ~p", [{E, R, ST}]),
            []
    end.

-spec source() -> binary().
source() ->
  <<"Gradualizer">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec load_dependencies() -> ok.
load_dependencies() ->
    Files = lists:flatmap(
              fun(Dir) -> filelib:wildcard(filename:join(Dir, "*.erl")) end,
              els_config:get(apps_paths) ++ els_config:get(deps_paths)),
    ok = gradualizer_db:import_erl_files(Files).

%% -spec dep_path(module()) -> string().
%% dep_path(Module) ->
%%   {ok, Uri} = els_utils:find_module(Module),
%%   els_utils:to_list(els_uri:path(Uri)).
