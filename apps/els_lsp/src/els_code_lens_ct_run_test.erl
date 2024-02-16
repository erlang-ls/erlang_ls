%%==============================================================================
%% Code Lens: ct_run_test
%%==============================================================================

-module(els_code_lens_ct_run_test).

-behaviour(els_code_lens).
-export([
    command/3,
    is_default/0,
    pois/1,
    precondition/1
]).

-spec command(els_dt_document:item(), els_poi:poi(), els_code_lens:state()) ->
    els_command:command().
command(#{uri := Uri} = _Document, POI, _State) ->
    #{id := {F, A}, range := #{from := {Line, _}}} = POI,
    Title = <<"Run test">>,
    CommandId = <<"ct-run-test">>,
    CommandArgs = [
        #{
            module => els_uri:module(Uri),
            function => F,
            arity => A,
            uri => Uri,
            line => Line
        }
    ],
    els_command:make_command(Title, CommandId, CommandArgs).

-spec is_default() -> boolean().
is_default() ->
    false.

-spec pois(els_dt_document:item()) -> [els_poi:poi()].
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

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec is_blacklisted(atom()) -> boolean().
is_blacklisted(Function) ->
    lists:member(Function, [init_per_suite, end_per_suite, group]).
