%%==============================================================================
%% Library to parse RFC-3986 URIs
%%==============================================================================
%% For details, see: http://tools.ietf.org/html/rfc3986
%%==============================================================================
-module(els_uri).

%%==============================================================================
%% Exports
%%==============================================================================
-export([
    module/1,
    path/1,
    uri/1
]).

%%==============================================================================
%% Types
%%==============================================================================
-type path() :: binary().

-export_type([path/0]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").

-spec module(uri()) -> atom().
module(Uri) ->
    binary_to_atom(filename:basename(path(Uri), <<".erl">>), utf8).

-spec path(uri()) -> path().
path(Uri) ->
    path(Uri, els_utils:is_windows()).

-spec path(uri(), boolean()) -> path().
path(Uri, IsWindows) ->
    #{
        host := Host,
        path := Path0,
        scheme := <<"file">>
    } = uri_string:normalize(Uri, [return_map]),
    Path = percent_decode(Path0),
    case {IsWindows, Host} of
        {true, <<>>} ->
            % Windows drive letter, have to strip the initial slash
            re:replace(
                Path, "^/([a-zA-Z]:)(.*)", "\\1\\2", [{return, binary}]
            );
        {true, _} ->
            <<"//", Host/binary, Path/binary>>;
        {false, <<>>} ->
            Path;
        {false, _} ->
            error(badarg)
    end.

-spec uri(path()) -> uri().
uri(Path) ->
    [Head | Tail] = filename:split(Path),
    {Host, Path1} =
        case {els_utils:is_windows(), Head} of
            {false, <<"/">>} ->
                {<<>>, uri_join(Tail)};
            {true, X} when X =:= <<"//">> orelse X =:= <<"\\\\">> ->
                [H | T] = Tail,
                {H, uri_join(T)};
            {true, _} ->
                % Strip the trailing slash from the first component
                H1 = <<(string:slice(Head, 0, 1))/binary, "%3A">>,
                {<<>>, uri_join([H1 | Tail])}
        end,

    els_utils:to_binary(
        uri_string:recompose(#{
            scheme => <<"file">>,
            host => Host,
            path => [$/, Path1]
        })
    ).

-spec uri_join([path()]) -> iolist().
uri_join(List) ->
    lists:join(<<"/">>, List).

-if(?OTP_RELEASE >= 23).
-spec percent_decode(binary()) -> binary().
percent_decode(Str) ->
    uri_string:percent_decode(Str).
-else.
-spec percent_decode(binary()) -> binary().
percent_decode(Str) ->
    http_uri:decode(Str).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

path_uri_test_() ->
    [
        ?_assertEqual(
            <<"/foo/bar.erl">>,
            path(<<"file:///foo/bar.erl">>)
        ),
        ?_assertEqual(
            <<"/foo/bar baz.erl">>,
            path(<<"file:///foo/bar%20baz.erl">>)
        ),
        ?_assertEqual(
            <<"/foo/bar.erl">>,
            path(uri(path(<<"file:///foo/bar.erl">>)))
        ),
        ?_assertEqual(
            <<"/foo/bar baz.erl">>,
            path(uri(<<"/foo/bar baz.erl">>))
        ),
        ?_assertEqual(
            <<"file:///foo/bar%20baz.erl">>,
            uri(<<"/foo/bar baz.erl">>)
        )
    ].

path_windows_test() ->
    ?assertEqual(
        <<"C:/foo/bar.erl">>,
        path(<<"file:///C%3A/foo/bar.erl">>, true)
    ).
-endif.
