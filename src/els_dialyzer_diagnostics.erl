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
-export([ is_default/0
        , run/1
        , source/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type macro_config() :: #{string() => string()}.
-type macro_option() :: {atom()} | {atom(), any()}.

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  true.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  Path = els_uri:path(Uri),
  case els_config:get(plt_path) of
    undefined -> [];
    DialyzerPltPath ->
      Deps  = [dep_path(X) || X <- els_diagnostics_utils:dependencies(Uri)],
      Files = [els_utils:to_list(Path) | Deps],
      WS = try dialyzer:run([ {files, Files}
                            , {from, src_code}
                            , {include_dirs, els_config:get(include_paths)}
                            , {plts, [DialyzerPltPath]}
                            , {defines, defines()}
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
-spec diagnostic({any(), {any(), integer()}, any()}) ->
        els_diagnostics:diagnostic().
diagnostic({_, {_, Line}, _} = Warning) ->
  Range    = els_protocol:range(#{ from => {Line, 1}
                                 , to   => {Line + 1, 1}
                                 }),
  Message0 = lists:flatten(dialyzer:format_warning(Warning)),
  Message  = els_utils:to_binary(Message0),
  #{ range    => Range
   , message  => Message
   , severity => ?DIAGNOSTIC_WARNING
   , source   => source()
   }.

-spec dep_path(module()) -> string().
dep_path(Module) ->
  {ok, Uri} = els_utils:find_module(Module),
  els_utils:to_list(els_uri:path(Uri)).

-spec defines() -> [macro_option()].
defines() ->
  Macros = els_config:get(macros),
  [define(M) || M <- Macros].

-spec define(macro_config()) -> macro_option().
define(#{"name" := Name, "value" := Value}) ->
  {list_to_atom(Name), els_utils:macro_string_to_term(Value)};
define(#{"name" := Name}) ->
  {list_to_atom(Name), true}.
