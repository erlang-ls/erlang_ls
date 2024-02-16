-module(els_call_hierarchy_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()}.
handle_request({prepare, Params}) ->
    {Uri, Line, Char} =
        els_text_document_position_params:uri_line_character(Params),
    {ok, Document} = els_utils:lookup_document(Uri),
    Functions = els_dt_document:wrapping_functions(Document, Line + 1, Char + 1),
    Items = [function_to_item(Uri, F) || F <- Functions],
    {response, Items};
handle_request({incoming_calls, Params}) ->
    #{<<"item">> := #{<<"uri">> := Uri} = Item} = Params,
    POI = els_call_hierarchy_item:poi(Item),
    References = els_references_provider:find_references(Uri, POI),
    Items = lists:flatten([reference_to_item(Reference) || Reference <- References]),
    {response, incoming_calls(Items)};
handle_request({outgoing_calls, Params}) ->
    #{<<"item">> := Item} = Params,
    #{<<"uri">> := Uri} = Item,
    POI = els_call_hierarchy_item:poi(Item),
    Applications = applications_in_function_range(Uri, POI),
    Items = lists:foldl(
        fun(Application, Acc) ->
            case application_to_item(Uri, Application) of
                {error, not_found} ->
                    %% The function may contain a reference
                    %% to a not yet implemented function
                    Acc;
                {ok, I} ->
                    [I | Acc]
            end
        end,
        [],
        Applications
    ),
    {response, outgoing_calls(lists:reverse(Items))}.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec incoming_calls([els_call_hierarchy_item:item()]) ->
    [els_call_hierarchy_item:incoming_call()].
incoming_calls(Items) ->
    [#{from => Item, fromRanges => [Range]} || #{range := Range} = Item <- Items].

-spec outgoing_calls([els_call_hierarchy_item:item()]) ->
    [els_call_hierarchy_item:outgoing_call()].
outgoing_calls(Items) ->
    [#{to => Item, fromRanges => [Range]} || #{range := Range} = Item <- Items].

-spec function_to_item(uri(), els_poi:poi()) -> els_call_hierarchy_item:item().
function_to_item(Uri, Function) ->
    #{id := Id, range := Range} = Function,
    Name = els_utils:function_signature(Id),
    Data = #{poi => Function},
    els_call_hierarchy_item:new(Name, Uri, Range, Range, Data).

-spec reference_to_item(location()) -> [els_call_hierarchy_item:item()].
reference_to_item(Reference) ->
    #{uri := RefUri, range := RefRange} = Reference,
    {ok, RefDoc} = els_utils:lookup_document(RefUri),
    case els_dt_document:wrapping_functions(RefDoc, RefRange) of
        [WrappingPOI] ->
            els_dt_document:wrapping_functions(RefDoc, RefRange),
            Name = els_utils:function_signature(maps:get(id, WrappingPOI)),
            POIRange = els_range:to_poi_range(RefRange),
            Data = #{poi => WrappingPOI},
            [els_call_hierarchy_item:new(Name, RefUri, POIRange, POIRange, Data)];
        _ ->
            []
    end.

-spec application_to_item(uri(), els_poi:poi()) ->
    {ok, els_call_hierarchy_item:item()} | {error, not_found}.
application_to_item(Uri, Application) ->
    #{id := Id} = Application,
    Name = els_utils:function_signature(Id),
    case els_code_navigation:goto_definition(Uri, Application) of
        {ok, [{DefUri, DefPOI} | _]} ->
            DefRange = maps:get(range, DefPOI),
            Data = #{poi => DefPOI},
            {ok, els_call_hierarchy_item:new(Name, DefUri, DefRange, DefRange, Data)};
        {error, Reason} ->
            {error, Reason}
    end.

-spec applications_in_function_range(uri(), els_poi:poi()) ->
    [els_poi:poi()].
applications_in_function_range(Uri, Function) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    #{data := #{wrapping_range := WrappingRange}} = Function,
    AllApplications = els_dt_document:pois(Document, ['application']),
    [
        A
     || #{range := AppRange} = A <- AllApplications,
        els_range:in(AppRange, WrappingRange)
    ].
