-module(els_inlay_hint_provider).

-behaviour(els_provider).

-export([
    handle_request/1,
    options/0
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Defines
%%==============================================================================

%%==============================================================================
%% Types
%%==============================================================================

%%==============================================================================
%% els_provider functions
%%==============================================================================
-spec handle_request(any()) -> {response, any()}.
handle_request({inlay_hint, Params}) ->
    #{<<"range">> := Range,
      <<"textDocument">> := #{<<"uri">> := Uri}
     } = Params,
    ?LOG_INFO("Inlay hint provider was called with params: ~p", [Params]),
    ?LOG_INFO("Range: ~p, uri: ~p", [Range, Uri]),
    {ok, [Document]} = els_dt_document:lookup(Uri),
    PoiRange = els_range:to_poi_range(Range),
    {response, get_inlay_hints(Document, PoiRange)}.

-spec options() -> boolean().
options() ->
    %% TODO
    true.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec get_inlay_hints(_, _) -> _.
get_inlay_hints(Document, Range) ->
    %% VarPOIs = els_dt_document:pois(Document, [variable]),
    AppPOIs = els_dt_document:pois(Document, [application]),
    ?LOG_INFO("APP POIS: ~p", [AppPOIs]),
    Matches = [A || #{range := AppR} = A <- AppPOIs,
                    els_range:in(AppR, Range)],

    Hints = [#{position => #{line => FromL-1, character => FromC-1},
              label => 'b'} || #{id := _Id, range := #{from := {FromL, FromC}}
                               } <- Matches],
    Hints2 = [#{position => #{line => L-1, character => C-1},
              label => 'e'} || #{id := _Id, range := #{to := {L, C}}
                               } <- Matches],
    foo(1, 2),
    Hints ++ Hints2.

-spec foo(_, _) -> _.
foo(A, B) ->
    A + B.
