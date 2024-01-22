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
    ?assertEqual(
        [
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"L1:">>,
                paddingRight => true,
                position => #{character => 17, line => 10}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"L2:">>,
                paddingRight => true,
                position => #{character => 21, line => 10}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"Foo:">>,
                paddingRight => true,
                position => #{character => 6, line => 9}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"Bar:">>,
                paddingRight => true,
                position => #{character => 9, line => 9}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"Foo:">>,
                paddingRight => true,
                position => #{character => 6, line => 8}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"A:">>,
                paddingRight => true,
                position => #{character => 6, line => 7}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"B:">>,
                paddingRight => true,
                position => #{character => 9, line => 7}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"Hej:">>,
                paddingRight => true,
                position => #{character => 6, line => 6}
            },
            #{
                kind => ?INLAY_HINT_KIND_PARAMETER,
                label => <<"Hoj:">>,
                paddingRight => true,
                position => #{character => 9, line => 6}
            }
        ],
        Result
    ),
    ok.
