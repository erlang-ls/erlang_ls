-module(els_folding_range_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").

-export([
    handle_request/1
]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type folding_range_result() :: [folding_range()] | null.

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(tuple()) -> {response, folding_range_result()}.
handle_request({document_foldingrange, Params}) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Document, [function, record]),
    Response =
        case
            [
                poi_range_to_folding_range(els_poi:folding_range(POI))
             || POI <- POIs, els_poi:folding_range(POI) =/= oneliner
            ]
        of
            [] -> null;
            Ranges -> Ranges
        end,
    {response, Response}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec poi_range_to_folding_range(els_poi:poi_range()) -> folding_range().
poi_range_to_folding_range(#{from := {FromLine, FromCol}, to := {ToLine, ToCol}}) ->
    #{
        startLine => FromLine - 1,
        startCharacter => FromCol,
        endLine => ToLine - 1,
        endCharacter => ToCol
    }.
