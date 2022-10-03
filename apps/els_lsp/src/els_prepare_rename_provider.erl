-module(els_prepare_rename_provider).

-behaviour(els_provider).

-export([
    handle_request/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
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
handle_request({prepare_rename, Params0}) ->
    #{
        <<"textDocument">> := #{<<"uri">> := Uri},
        <<"position">> := #{
            <<"line">> := Line,
            <<"character">> := Character
        }
    } = Params0,
    case els_utils:lookup_document(Uri) of
        {ok, Document} ->
            Params = Params0#{<<"newName">> => <<"newName">>},
            POIs = els_dt_document:get_element_at_pos(
                Document,
                Line + 1,
                Character + 1
            ),
            case POIs of
                [POI | _] ->
                    try
                        els_provider:handle_request(
                            els_rename_provider,
                            {rename, Params}
                        )
                    of
                        {response, null} ->
                            {response, null};
                        {response, _} ->
                            {response, els_protocol:range(rename_range(POI))}
                    catch
                        Class:Reason:Stacktrace ->
                            ?LOG_ERROR(
                                "prepareRenamed failed: ~p:~p\n"
                                "Stacktrace:\n~p\n",
                                [Class, Reason, Stacktrace]
                            ),
                            {response, null}
                    end;
                _ ->
                    {response, null}
            end;
        {error, Error} ->
            ?LOG_WARNING("Failed to read uri: ~p ~p", [Error, Uri]),
            {response, null}
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec rename_range(els_poi:poi()) -> els_poi:poi_range().
rename_range(#{data := #{name_range := Range}}) ->
    Range;
rename_range(#{kind := Kind, range := #{from := {FromL, FromC}, to := To}}) when
    Kind =:= macro;
    Kind =:= record_expr
->
    %% Don't include # or ? in name..
    #{from => {FromL, FromC + 1}, to => To};
rename_range(#{range := Range}) ->
    Range.
