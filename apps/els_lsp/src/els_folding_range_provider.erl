-module(els_folding_range_provider).

-behaviour(els_provider).

-include("els_lsp.hrl").

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type folding_range_result() :: [folding_range()] | null.
-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(tuple(), state()) -> {folding_range_result(), state()}.
handle_request({document_foldingrange, Params}, State) ->
  #{ <<"textDocument">> := #{<<"uri">> := Uri} } = Params,
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_dt_document:pois(Document, [function, record]),
  FoldingRanges = [folding_range(Range) ||
                   #{data := #{folding_range := Range = #{}}} <- POIs],
  Response = case FoldingRanges of
               []     -> null;
               Ranges -> Ranges
             end,
  {Response, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec folding_range(poi_range()) -> folding_range().
folding_range(#{from := {FromLine, FromCol}, to := {ToLine, ToCol}}) ->
  #{ startLine      => FromLine - 1
   , startCharacter => FromCol
   , endLine        => ToLine - 1
   , endCharacter   => ToCol
   }.
