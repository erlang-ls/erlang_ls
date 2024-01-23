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
    Job = run_inlay_hints_job(Document, PoiRange),
    {async, Uri, Job}.

-spec options() -> boolean().
options() ->
    %% TODO
    true.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec run_inlay_hints_job(_, _) -> _.
run_inlay_hints_job(Document, Range) ->
    Config = #{
               task => fun get_inlay_hints/2,
               entries => [{Document, Range}],
               title => <<"Inlay Hints">>,
               on_complete =>
                   fun(Resp) ->
                           els_server ! {result, Resp, self()},
                           ok
                   end
              },
    {ok, Pid} = els_background_job:new(Config),
    Pid.


-spec get_inlay_hints(_, _) -> _.
get_inlay_hints({Document, Range}, _) ->
    ?LOG_INFO("BEGIN"),
    %% VarPOIs = els_dt_document:pois(Document, [variable]),
    AppPOIs = els_dt_document:pois(Document, [application]),
    #{uri := Uri} = Document,
    Matches = [{A, definition(Uri, A)} ||
                  #{range := AppR} = A <- AppPOIs,
                    els_range:in(AppR, Range)],

    Hints0 = [{Id, {N, Pos}, lists:keyfind(N, 1, Args)} ||
                {#{id := Id, data := #{args2 := Args2}},
                 {_Uri, #{data := #{args := Args}}}} <- Matches,
                {N, Name, Pos} <- Args2,
             lists:keyfind(N, 1, Args) /= {N, Name},
             not is_unnamed(lists:keyfind(N, 1, Args))],
    Hints =  [#{position => #{line => FromL-1,
                              character => FromC-1},
                label => unicode:characters_to_binary(ArgName ++ " ="),
                paddingRight => true}
              || {_Id, {N, #{from := {FromL, FromC}}}, {N, ArgName}} <- Hints0],
    ?LOG_INFO("DONE ~p", [length(Hints)]),
    Hints.

-spec is_unnamed(_) -> boolean().
is_unnamed({_, Name}) ->
    %% TODO: Use atom in name instead of matching on prefix
    nomatch /= string:prefix(Name, "Arg").


-spec definition(_, _) -> _.
definition(Uri, A) ->
    case els_code_navigation:goto_definition(Uri, A) of
        {ok, [Def|_]} -> Def;
        Err ->
            ?LOG_INFO("Error: ~p ~p", [Err, A]),
            error
    end.

%% -spec foo(_, _) -> _.
%% foo(A, B) ->
%%     A + B.
