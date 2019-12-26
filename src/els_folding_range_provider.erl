-module(els_folding_range_provider).

-behaviour(els_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type folding_range() :: #{ startLine      := pos_integer()
                          , startCharacter := pos_integer()
                          , endLine        := pos_integer()
                          , endCharacter   := pos_integer()
                          }.
-type folding_range_result() :: [folding_range()] | null.

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(tuple(), els_provider:state()) ->
   {folding_range_result(), els_provider:state()}.
handle_request({document_foldingrange, Params}, State) ->
  #{ <<"textDocument">> := #{<<"uri">> := Uri} } = Params,
  {ok, [Document]} = els_dt_document:lookup(Uri),
  POIs = els_dt_document:pois(Document, [folding_range]),
  Response = case [folding_range(POI) || POI <- POIs] of
               []     -> null;
               Ranges -> Ranges
             end,
  {Response, State}.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec folding_range(poi()) -> folding_range().
folding_range(#{range := #{from := {FromL, FromC}, to := {ToL, ToC}}}) ->
  #{ startLine      => FromL
   , startCharacter => FromC
   , endLine        => ToL
   , endCharacter   => ToC
   }.
