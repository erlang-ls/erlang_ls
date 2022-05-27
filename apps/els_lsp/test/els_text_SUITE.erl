-module(els_text_SUITE).

%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    apply_edits_single_line/1,
    apply_edits_multi_line/1,
    apply_edits_unicode/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {seconds, 30}}].

-spec all() -> [atom()].
all() ->
    els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
    els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
    Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
    Config.

%%==============================================================================
%% Testcases
%%==============================================================================
apply_edits_single_line(_Config) ->
    In = <<"a b c">>,
    C = fun(FromC, ToC, Str) -> {#{from => {0, FromC}, to => {0, ToC}}, Str} end,
    F = fun els_text:apply_edits/2,
    ?assertEqual(<<"a b c">>, F(In, [])),
    ?assertEqual(<<"a b c">>, F(In, [C(0, 0, "")])),
    ?assertEqual(<<"_a b c">>, F(In, [C(0, 0, "_")])),
    ?assertEqual(<<"_ b c">>, F(In, [C(0, 1, "_")])),
    ?assertEqual(<<"__ b c">>, F(In, [C(0, 1, "__")])),
    ?assertEqual(<<"__b c">>, F(In, [C(0, 2, "__")])),
    ?assertEqual(<<"a _ c">>, F(In, [C(2, 3, "_")])),
    ?assertEqual(<<"c">>, F(In, [C(0, 4, "")])),
    ?assertEqual(<<"a">>, F(In, [C(1, 5, "")])),
    ?assertEqual(<<"">>, F(In, [C(0, 5, "")])),
    ?assertEqual(<<"a b c!">>, F(In, [C(5, 5, "!")])),
    ?assertEqual(
        <<>>,
        F(<<>>, [
            C(0, 0, ""),
            C(0, 0, "")
        ])
    ),
    ?assertEqual(
        <<"cba">>,
        F(<<>>, [
            C(0, 0, "a"),
            C(0, 0, "b"),
            C(0, 0, "c")
        ])
    ),
    ?assertEqual(
        <<"abc">>,
        F(<<>>, [
            C(0, 0, "c"),
            C(0, 0, "b"),
            C(0, 0, "a")
        ])
    ),
    ?assertEqual(
        <<"ab">>,
        F(In, [
            C(0, 0, "a"),
            C(1, 6, ""),
            C(1, 1, "b")
        ])
    ),
    ?assertEqual(
        <<"aba b c">>,
        F(In, [
            C(0, 0, "a"),
            C(1, 1, "b")
        ])
    ),
    ?assertEqual(
        <<"_ c!">>,
        F(In, [
            C(0, 3, ""),
            C(0, 0, "_"),
            C(3, 3, "!")
        ])
    ),
    ok.

apply_edits_multi_line(_Config) ->
    In = <<
        "a b c\n"
        "d e f\n"
        "g h i\n"
    >>,
    C = fun(FromL, FromC, ToL, ToC, Str) ->
        {#{from => {FromL, FromC}, to => {ToL, ToC}}, Str}
    end,
    F = fun els_text:apply_edits/2,
    ?assertEqual(In, F(In, [])),
    ?assertEqual(In, F(In, [C(0, 0, 0, 0, "")])),
    ?assertEqual(<<"_ b c\n", "d e f\n", "g h i\n">>, F(In, [C(0, 0, 0, 1, "_")])),
    ?assertEqual(<<"_a b c\n", "d e f\n", "g h i\n">>, F(In, [C(0, 0, 0, 0, "_")])),
    ?assertEqual(<<"a b c\n", "_d e f\n", "g h i\n">>, F(In, [C(1, 0, 1, 0, "_")])),
    ?assertEqual(<<"a b c\n", "d e f\n", "_g h i\n">>, F(In, [C(2, 0, 2, 0, "_")])),
    ?assertEqual(<<"a b c\n", "d e f\n", "g h i_\n">>, F(In, [C(2, 5, 2, 5, "_")])),
    ?assertEqual(<<"a b c\n", "d e f\n", "g h _\n">>, F(In, [C(2, 4, 2, 5, "_")])),
    ?assertEqual(<<"a b c\n", "d e f\n", "g _ i\n">>, F(In, [C(2, 2, 2, 3, "_")])),
    ?assertEqual(<<"a b c\n", "d e f\n", "g h i\n", "_">>, F(In, [C(3, 0, 3, 0, "_")])),
    ?assertEqual(<<"_">>, F(In, [C(0, 0, 3, 0, "_")])),
    ?assertEqual(
        <<"a b _\n", "d e _\n", "g h _\n">>,
        F(In, [
            C(0, 4, 0, 5, "_"),
            C(1, 4, 1, 5, "_"),
            C(2, 4, 2, 5, "_")
        ])
    ),
    ?assertEqual(<<"a b c\n", "g h i\n">>, F(In, [C(1, 0, 2, 0, "")])),
    ?assertEqual(<<"a b h i\n">>, F(In, [C(0, 3, 2, 1, "")])),
    ?assertEqual(
        <<"a b _ i\n">>,
        F(In, [
            C(0, 3, 2, 1, ""),
            C(0, 4, 0, 5, "_")
        ])
    ),
    ?assertEqual(
        <<"a b h _\n">>,
        F(In, [
            C(0, 3, 2, 1, ""),
            C(0, 6, 0, 7, "_")
        ])
    ),
    ?assertEqual(
        <<"a ba\n", "_\nc\n", " h i\n">>,
        F(In, [
            C(0, 3, 2, 1, "a\nb\nc\n"),
            C(1, 0, 1, 1, "_")
        ])
    ),
    ok.

apply_edits_unicode(_Config) ->
    In = <<"二郎"/utf8>>,
    C = fun(FromC, ToC, Str) -> {#{from => {0, FromC}, to => {0, ToC}}, Str} end,
    F = fun els_text:apply_edits/2,
    ?assertEqual(<<"㇐郎"/utf8>>, F(In, [C(0, 1, "㇐")])),
    ?assertEqual(<<"二㇐郎"/utf8>>, F(In, [C(1, 1, "㇐")])),
    ?assertEqual(<<"二㇐"/utf8>>, F(In, [C(1, 2, "㇐")])),
    ?assertEqual(<<"二郎㇐"/utf8>>, F(In, [C(2, 2, "㇐")])),
    ?assertEqual(<<"二郎㇐"/utf8>>, F(In, [C(2, 2, "㇐")])),
    ?assertEqual(<<"㇐二郎"/utf8>>, F(In, [C(0, 0, "㇐")])),
    ok.
