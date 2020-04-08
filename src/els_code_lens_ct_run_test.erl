%%==============================================================================
%% Code Lens: ct_run_test
%%==============================================================================

-module(els_code_lens_ct_run_test).

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
  <<"ct-run-test">>.

-spec command_args(els_dt_document:item(), poi()) -> [any()].
command_args( #{uri := Uri} = _Document
            , #{id := {F, A}, range := #{from := {Line, _}}} = _POI) ->
  [#{ module => els_uri:module(Uri)
    , function => F
    , arity => A
    , uri => Uri
    , line => Line
    }].

-spec is_default() -> boolean().
is_default() ->
  false.

-spec pois(els_dt_document:item()) -> [poi()].
pois(Document) ->
  Functions = els_dt_document:pois(Document, [function]),
  [POI || #{id := {F, 1}} = POI <- Functions, not is_blacklisted(F)].

-spec precondition(els_dt_document:item()) -> boolean().
precondition(Document) ->
  Includes = els_dt_document:pois(Document, [include_lib]),
  case [POI || #{id := "common_test/include/ct.hrl"} = POI <- Includes] of
    [] ->
      false;
    _ ->
      true
  end.

-spec title(poi()) -> binary().
title(_POI) ->
  <<"Run test">>.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec is_blacklisted(atom()) -> boolean().
is_blacklisted(Function) ->
  lists:member(Function, [init_per_suite, end_per_suite, group]).
