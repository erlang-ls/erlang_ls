%%==============================================================================
%% Code Lens: show_behaviour_usages
%%==============================================================================

-module(els_code_lens_show_behaviour_usages).

-behaviour(els_code_lens).
-export([ command/1
        , command_args/2
        , is_default/0
        , pois/1
        , precondition/1
        , title/1
        ]).

-include("erlang_ls.hrl").

-spec command(poi()) -> els_command:command_id().
command(_POI) ->
  <<"show-behaviour-usages">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args(_Document, _POI) ->
  [].

-spec is_default() -> boolean().
is_default() ->
  true.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(Document) ->
  %% A behaviour is defined by the presence of one more callback
  %% attributes.
  Callbacks = els_dt_document:pois(Document, [callback]),
  length(Callbacks) > 0.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  els_dt_document:pois(Document, [module]).

-spec title(poi()) -> binary().
title(#{ id := Id }) ->
  {ok, Refs} = els_dt_references:find_by_id(behaviour, Id),
  Count = length(Refs),
  Msg = io_lib:format("Behaviour used in ~p place(s)", [Count]),
  els_utils:to_binary(Msg).
