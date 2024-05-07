-module(els_inlay_hint_SUITE).

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
    basic/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("els_lsp.hrl").

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
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config),
    ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec basic(config()) -> ok.
basic(Config) ->
    Uri = ?config(inlay_hint_uri, Config),
    Range = #{
        start => #{line => 0, character => 0},
        'end' => #{line => 999, character => 0}
    },
    #{result := Result} = els_client:inlay_hint(Uri, Range),
    assert_result(
        [
            #{
                label => <<"List1:">>,
                position => #{line => 13, character => 17},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"List2:">>,
                position => #{line => 13, character => 21},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"G1:">>,
                position => #{line => 12, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"G2:">>,
                position => #{line => 12, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },

            #{
                label => <<"F1:">>,
                position => #{line => 11, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"F2:">>,
                position => #{line => 11, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"E1:">>,
                position => #{line => 10, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"E2:">>,
                position => #{line => 10, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"D1:">>,
                position => #{line => 9, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"D2:">>,
                position => #{line => 9, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"Foo:">>,
                position => #{line => 8, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"B1:">>,
                position => #{line => 7, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"B2:">>,
                position => #{line => 7, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"A1:">>,
                position => #{line => 6, character => 6},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"A2:">>,
                position => #{line => 6, character => 9},
                kind => ?INLAY_HINT_KIND_PARAMETER,
                paddingRight => true
            },
            #{
                label => <<"exp">>,
                position => #{line => 5, character => 0},
                kind => ?INLAY_HINT_KIND_TYPE,
                paddingRight => true
            }
        ],
        Result
    ),
    ok.

assert_result([], []) ->
    ok;
assert_result([Same | ExpectRest], [Same | ResultRest]) ->
    assert_result(ExpectRest, ResultRest);
assert_result([Expect | _], [Result | _]) ->
    ?assertEqual(Expect, Result).
