-module(els_include_paths).
-export([includes/1]).
-export([include_libs/0]).
-export([include_libs/1]).

-include_lib("els_core/include/els_core.hrl").

-spec includes(els_dt_document:item()) -> [binary()].
includes(#{uri := Uri}) ->
    case match_in_path(els_uri:path(Uri), els_config:get(apps_paths)) of
        [] ->
            [];
        [Path | _] ->
            AppPath = filename:join(lists:droplast(filename:split(Path))),
            {ok, Headers} = els_dt_document_index:find_by_kind(header),
            lists:flatmap(
                fun(#{uri := HeaderUri}) ->
                    case string:prefix(els_uri:path(HeaderUri), AppPath) of
                        nomatch ->
                            [];
                        IncludePath ->
                            [relative_include_path(IncludePath)]
                    end
                end,
                Headers
            )
    end.

-spec include_libs() -> [binary()].
include_libs() ->
    {ok, Headers} = els_dt_document_index:find_by_kind(header),
    Uris = [Uri || #{uri := Uri} <- Headers],
    include_libs(Uris).

-spec include_libs([uri()]) -> [binary()].
include_libs(Uris) ->
    Paths =
        els_config:get(otp_paths) ++
            els_config:get(deps_paths) ++
            els_config:get(apps_paths) ++
            els_config:get(include_paths),
    lists:flatmap(
        fun(Uri) ->
            HeaderPath = els_uri:path(Uri),
            case match_in_path(HeaderPath, Paths) of
                [] ->
                    [];
                [Path | _] ->
                    <<"/", PathSuffix/binary>> = string:prefix(HeaderPath, Path),
                    PathBin = unicode:characters_to_binary(Path),
                    case lists:reverse(filename:split(PathBin)) of
                        [<<"include">>, App | _] ->
                            [
                                filename:join([
                                    strip_app_version(App),
                                    <<"include">>,
                                    PathSuffix
                                ])
                            ];
                        _ ->
                            []
                    end
            end
        end,
        Uris
    ).

-spec match_in_path(binary(), [binary()]) -> [binary()].
match_in_path(DocumentPath, Paths) ->
    [P || P <- Paths, string:prefix(DocumentPath, P) =/= nomatch].

-spec relative_include_path(binary()) -> binary().
relative_include_path(Path) ->
    case filename:split(Path) of
        [_App, <<"include">> | Rest] -> filename:join(Rest);
        [_App, <<"src">> | Rest] -> filename:join(Rest);
        [_App, SubDir | Rest] -> filename:join([<<"..">>, SubDir | Rest])
    end.

-spec strip_app_version(binary()) -> binary().
strip_app_version(App0) ->
    %% Transform "foo-1.0" into "foo"
    case string:lexemes(App0, "-") of
        [] ->
            App0;
        [_] ->
            App0;
        Lexemes ->
            Vsn = lists:last(Lexemes),
            case re:run(Vsn, "^[0-9.]+$", [global, {capture, none}]) of
                match -> list_to_binary(lists:join("-", lists:droplast(Lexemes)));
                nomatch -> App0
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

strip_app_version_test() ->
    ?assertEqual(<<"foo">>, strip_app_version(<<"foo">>)),
    ?assertEqual(<<"foo">>, strip_app_version(<<"foo-1.2.3">>)),
    ?assertEqual(<<"">>, strip_app_version(<<"">>)),
    ?assertEqual(<<"foo-bar">>, strip_app_version(<<"foo-bar">>)),
    ?assertEqual(<<"foo-bar">>, strip_app_version(<<"foo-bar-1.2.3">>)),
    ?assertEqual(<<"foo-bar-baz">>, strip_app_version(<<"foo-bar-baz">>)),
    ?assertEqual(<<"foo-bar-baz">>, strip_app_version(<<"foo-bar-baz-1.2.3">>)).

-endif.
