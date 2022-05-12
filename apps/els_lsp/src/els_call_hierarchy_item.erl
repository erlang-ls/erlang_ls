-module(els_call_hierarchy_item).

-include("els_lsp.hrl").

-include_lib("kernel/include/logger.hrl").

-export([
    new/5,
    poi/1
]).

-type data() :: any().
-type item() :: #{
    name := binary(),
    kind := symbol_kind(),
    tags => [symbol_tag()],
    detail => binary(),
    uri := uri(),
    range := range(),
    selectionRange := range(),
    data => data()
}.
-type incoming_call() :: #{
    from := item(),
    fromRanges := [range()]
}.
-type outgoing_call() :: #{
    to := item(),
    fromRanges := [range()]
}.
-export_type([
    item/0,
    incoming_call/0,
    outgoing_call/0
]).

%% @doc Extract and decode the POI from the data
-spec poi(item()) -> poi().
poi(#{<<"data">> := Data}) ->
    maps:get(poi, els_utils:base64_decode_term(Data)).

-spec new(binary(), uri(), poi_range(), poi_range(), data()) -> item().
new(Name, Uri, Range, SelectionRange, Data) ->
    #{from := {StartLine, _}} = Range,
    Detail = <<
        (atom_to_binary(els_uri:module(Uri), utf8))/binary,
        " [L",
        (integer_to_binary(StartLine))/binary,
        "]"
    >>,
    #{
        name => Name,
        kind => ?SYMBOLKIND_FUNCTION,
        detail => Detail,
        uri => Uri,
        range => els_protocol:range(Range),
        selectionRange => els_protocol:range(SelectionRange),
        data => els_utils:base64_encode_term(Data)
    }.
