%%==============================================================================
%% Code Lens: suggest_spec
%%==============================================================================
-module(els_code_lens_suggest_spec).

-behaviour(els_code_lens).
-export([ command/1
        , command_args/2
        , is_default/0
        , pois/1
        , precondition/1
        , title/2
        ]).

-include("els_lsp.hrl").

-spec command(poi()) -> els_command:command_id().
command(_POI) ->
  <<"suggest-spec">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args( #{uri := Uri} = _Document
            , #{id := {F, A}, range := #{from := {Line, _}}} = _POI) ->
  [#{ function => F
    , arity => A
    , uri => Uri
    , line => Line
    }].

-spec is_default() -> boolean().
is_default() ->
  true.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  Functions = els_dt_document:pois(Document, [function]),
  Specs = els_dt_document:pois(Document, [spec]),
  SpecsIds = [Id || #{id := Id} <- Specs],
  [POI || #{id := Id} = POI <- Functions, not lists:member(Id, SpecsIds)].

-spec precondition(els_dt_document:item()) -> boolean().
precondition(_Document) ->
  true.

-spec title(els_dt_document:item(), poi()) -> binary().
title(_Title, _POI) ->
  <<"Add spec">>.
