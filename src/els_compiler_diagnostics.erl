%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_compiler_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(els_diagnostics).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ diagnostics/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(COMPILER_OPTS, [ return_warnings
                       , return_errors
                       , basic_validation
                       ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type compiler_info() :: {erl_anno:line() | 'none', module(), any()}.
-type compiler_msg()  :: {file:filename(), [compiler_info()]}.

%%==============================================================================
%% Callback Functions
%%==============================================================================
-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      compile(Uri);
    _Ext ->
      lager:debug("Unsupported extension during compilation [uri=~p]", [Uri]),
      []
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec compile(uri()) -> [diagnostic()].
compile(Uri) ->
  Path = els_uri:path(Uri),
  Includes = [ {i, IncludePath}
               || IncludePath <- els_config:get(include_paths)
             ],
  case compile:file(binary_to_list(Path), Includes ++ ?COMPILER_OPTS) of
    {ok, _, WS} ->
      diagnostics(WS, ?DIAGNOSTIC_WARNING);
    {error, ES, WS} ->
      diagnostics(WS, ?DIAGNOSTIC_WARNING) ++ diagnostics(ES, ?DIAGNOSTIC_ERROR)
  end.


-spec diagnostics([compiler_msg()], severity()) -> [diagnostic()].
diagnostics(List, Severity) ->
  lists:flatten([[ diagnostic(location(Line), Module, Desc, Severity)
                   || {Line, Module, Desc} <- Info]
                 || {_Filename, Info } <- List]).

-spec diagnostic(erl_anno:location(), module(), string(), integer()) ->
  diagnostic().
diagnostic({Line, Col}, Module, Desc, Severity) ->
  Range   = #{from => {Line, Col}, to => {Line + 1, 1}},
  Message = list_to_binary(lists:flatten(Module:format_error(Desc))),
  #{ range    => els_protocol:range(Range)
   , message  => Message
   , severity => Severity
   }.

-spec location(erl_anno:line() | none) -> erl_anno:location().
location(Line) when is_integer(Line) ->
  {Line, 1};
location(none) ->
  {1, 1}.
