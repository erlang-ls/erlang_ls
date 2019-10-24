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
-spec diagnostic({any(), {any(), integer()}, any()}) -> diagnostic().
diagnostic({_, {_, Line}, _} = Warning) ->
  Range   = erlang_ls_protocol:range(#{ from => {Line - 1, 0}
                                      , to   => {Line - 1, 0}
                                      }),
  Message = list_to_binary(lists:flatten(dialyzer:format_warning(Warning))),
  #{ range    => Range
   , message  => Message
   , severity => ?DIAGNOSTIC_WARNING
   }.
