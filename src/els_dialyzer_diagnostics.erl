%%==============================================================================
%% Compiler diagnostics
%%==============================================================================
-module(els_dialyzer_diagnostics).

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
%% Callback Functions
%%==============================================================================
-spec diagnostics(uri()) -> [diagnostic()].
diagnostics(Uri) ->
  Path = els_uri:path(Uri),
  case els_config:get(plt_path) of
    undefined -> [];
    DialyzerPltPath ->
      WS = try dialyzer:run([ {files, [binary_to_list(Path)]}
                            , {from, src_code}
                            , {plts, [DialyzerPltPath]}
                            ])
           catch _:_ -> []
           end,
      [diagnostic(W) || W <- WS]
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec diagnostic({any(), {any(), integer()}, any()}) -> diagnostic().
diagnostic({_, {_, Line}, _} = Warning) ->
  Range   = els_protocol:range(#{ from => {Line, 0}
                                , to   => {Line, 0}
                                }),
  Message = list_to_binary(lists:flatten(dialyzer:format_warning(Warning))),
  #{ range    => Range
   , message  => Message
   , severity => ?DIAGNOSTIC_WARNING
   }.
