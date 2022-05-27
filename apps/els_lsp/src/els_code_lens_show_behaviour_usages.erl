%%==============================================================================
%% Code Lens: show_behaviour_usages
%%==============================================================================

-module(els_code_lens_show_behaviour_usages).

-behaviour(els_code_lens).
-export([
    command/3,
    is_default/0,
    pois/1,
    precondition/1
]).

-spec command(els_dt_document:item(), els_poi:poi(), els_code_lens:state()) ->
    els_command:command().
command(_Document, POI, _State) ->
    Title = title(POI),
    CommandId = <<"show-behaviour-usages">>,
    CommandArgs = [],
    els_command:make_command(Title, CommandId, CommandArgs).

-spec is_default() -> boolean().
is_default() ->
    true.

-spec precondition(els_dt_document:item()) -> boolean().
precondition(Document) ->
    %% A behaviour is defined by the presence of one more callback
    %% attributes.
    Callbacks = els_dt_document:pois(Document, [callback]),
    length(Callbacks) > 0.

-spec pois(els_dt_document:item()) -> [els_poi:poi()].
pois(Document) ->
    els_dt_document:pois(Document, [module]).

-spec title(els_poi:poi()) -> binary().
title(#{id := Id} = _POI) ->
    {ok, Refs} = els_dt_references:find_by_id(behaviour, Id),
    Count = length(Refs),
    Msg = io_lib:format("Behaviour used in ~p place(s)", [Count]),
    els_utils:to_binary(Msg).
