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
-export([ diagnostics/1
        , source/0
        ]).

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
      Dependencies = deps_paths(els_diagnostics_utils:dependencies(Uri)),
      WS = try dialyzer:run([ {files, [binary_to_list(Path) | Dependencies]}
                            , {from, src_code}
                            , {include_dirs, els_config:get(include_paths)}
                            , {plts, [DialyzerPltPath]}
                            ])
           catch Type:Error ->
               lager:error( "Error while running dialyzer [type=~p] [error=~p]"
                          , [Type, Error]
                          ),
               []
           end,
      [diagnostic(W) || W <- WS]
  end.

-spec source() -> binary().
source() ->
  <<"Dialyzer">>.

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
   , source   => source()
   }.

-spec deps_paths([diagnostic()]) -> [els_uri:path()].
deps_paths(Dependencies) ->
  lists:map(fun(Dependency) ->
                {ok, Uri} = els_utils:find_module(Dependency),
                els_uri:path(Uri)
            end, Dependencies).
