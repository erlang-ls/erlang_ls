-module(els_folding_range_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").

-export([
    is_enabled/0,
    handle_request/2
]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type folding_range_result() :: [folding_range()] | null.

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(tuple(), any()) -> {response, folding_range_result()}.
handle_request({document_foldingrange, Params}, _State) ->
    #{<<"textDocument">> := #{<<"uri">> := Uri}} = Params,
    {ok, Document} = els_utils:lookup_document(Uri),
    POIs = els_dt_document:pois(Document, [function, record]),
    Response =
        case
            [
                folding_range(Range)
             || #{data := #{folding_range := Range = #{}}} <- POIs
            ]
        of
            [] -> null;
            Ranges -> Ranges
        end,
    {response, Response}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec folding_range(els_poi:poi_range()) -> folding_range().
folding_range(#{from := {FromLine, FromCol}, to := {ToLine, ToCol}}) ->
    #{
        startLine => FromLine - 1,
        startCharacter => FromCol,
        endLine => ToLine - 1,
        endCharacter => ToCol
    }.
