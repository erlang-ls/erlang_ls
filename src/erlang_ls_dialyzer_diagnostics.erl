%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(erlang_ls_dialyzer_diagnostics).

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
%% Callback Functions
%%==============================================================================
-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  Path = erlang_ls_uri:path(Uri),
  WS = try dialyzer:run([{files, [binary_to_list(Path)]}, {from, src_code}])
       catch _:_ ->
           []
       end,
  [diagnostic(W) || W <- WS].

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec diagnostic(any()) -> diagnostic().
diagnostic({_, {_, Line}, _} = Warning) ->
  Range   = erlang_ls_protocol:range(Line),
  Message = list_to_binary(lists:flatten(dialyzer:format_warning(Warning))),
  #{ range    => Range
   , message  => Message
   , severity => ?DIAGNOSTIC_WARNING
   }.
