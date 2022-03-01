%%==============================================================================
%% RefactorErl Diagnostics
%%==============================================================================
-module(els_refactorerl_diagnostics).

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
%% Includes & Defines
%%==============================================================================
-include("els_lsp.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type refactorerl_diagnostic_alias() :: atom().
-type refactorerl_diagnostic_result() :: {range(), string()}.
%-type refactorerl_query() :: [char()].

%%==============================================================================
%% Callback Functions
%%==============================================================================

-spec is_default() -> boolean().
is_default() ->
  false.

-spec run(uri()) -> [els_diagnostics:diagnostic()].
run(Uri) ->
  case filename:extension(Uri) of
    <<".erl">> ->
      case els_refactorerl_utils:referl_node() of
        {error, _} ->
          [];
        {ok, _} ->
          case els_refactorerl_utils:add(Uri) of
            error ->
              [];
            ok ->
              Module = els_uri:module(Uri),
              Diags = enabled_diagnostics(),
              Results = els_refactorerl_utils:run_diagnostics(Diags, Module),
              make_diagnostics(Results)
          end
      end;
    _ ->
      []
  end.

-spec source() -> binary().
source() ->
  els_refactorerl_utils:source_name().

%%==============================================================================
%% Internal Functions
%%==============================================================================

% @doc
% Returns the enabled diagnostic aliases from config
%-spec configured_diagnostics() -> sets:set().
%configured_diagnostics() ->
%  case els_config:get(refactorerl) of
%    #{"diagnostics" := List} ->
%      sets:from_list(List);
%    _ ->
%      []
%  end.

% @doc
% Returns the default diagnostic aliases
%-spec default_diagnostics() -> sets:set().
%default_diagnostics() ->
%  sets:from_list(["unused_macros", "unsecure_os_call"]).


% @doc
% Returns the enabled diagnostics by merging default and configed
-spec enabled_diagnostics() -> [refactorerl_diagnostic_alias()].
enabled_diagnostics() ->
  %Set = sets:union(default_diagnostics(), configured_diagnostics()),
  %sets:to_list(Set), %TODO Set operation
  %["unused_macros", "unsecure_os_call"].
  [unused_macros, unsecure_os_call].


% @doc
% Constructs the ELS diagnostic from RefactorErl result
-spec make_diagnostics([refactorerl_diagnostic_result()]) -> any().
make_diagnostics([{Range, Message} | Tail]) ->
  Severity = ?DIAGNOSTIC_WARNING,
  Source = source(),
  Diag = els_diagnostics:make_diagnostic(Range, Message, Severity, Source),
  [ Diag | make_diagnostics(Tail) ];

make_diagnostics([]) ->
  [].
