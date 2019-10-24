%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(erlang_ls_compiler_diagnostics).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(erlang_ls_diagnostics).

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
-define(COMPILER_OPTS, [debug_info, return_warnings, return_errors]).

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
  Path = erlang_ls_uri:path(Uri),
  case compile:file(binary_to_list(Path), ?COMPILER_OPTS) of
    {ok, _, WS} ->
      diagnostics(WS, ?DIAGNOSTIC_WARNING);
    {error, ES, WS} ->
      diagnostics(WS, ?DIAGNOSTIC_WARNING) ++ diagnostics(ES, ?DIAGNOSTIC_ERROR)
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec diagnostics([compiler_msg()], severity()) -> [diagnostic()].
diagnostics(List, Severity) ->
  lists:flatten([[ diagnostic(Line, Module, Desc, Severity)
                   || {Line, Module, Desc} <- Info]
                 || {_Filename, Info } <- List]).

-spec diagnostic(integer(), module(), string(), integer()) -> diagnostic().
diagnostic(Line, Module, Desc, Severity) ->
  Range   = case is_integer(Line) of
              true  -> #{from => {Line, 1}, to => {Line, 1}};
              false -> #{from => {1, 1}, to => {1, 1}}
            end,
  Message = list_to_binary(lists:flatten(Module:format_error(Desc))),
  #{ range    => erlang_ls_protocol:range(Range)
   , message  => Message
   , severity => Severity
   }.
