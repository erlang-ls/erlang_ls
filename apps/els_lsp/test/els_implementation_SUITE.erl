-module(els_implementation_SUITE).

-include("els_lsp.hrl").

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
    gen_server_call/1,
    callback/1,
    dynamic_call/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
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
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec gen_server_call(config()) -> ok.
gen_server_call(Config) ->
    Uri = ?config(my_gen_server_uri, Config),
    #{result := Result} = els_client:implementation(Uri, 30, 10),
    Expected = [
        #{
            range =>
                #{
                    'end' => #{character => 4, line => 46},
                    start => #{character => 0, line => 46}
                },
            uri => Uri
        }
    ],
    ?assertEqual(Expected, Result),
    ok.

-spec callback(config()) -> ok.
callback(Config) ->
    Uri = ?config(implementation_uri, Config),
    #{result := Result} = els_client:implementation(Uri, 3, 20),
    Expected = [
        #{
            range =>
                #{
                    'end' => #{character => 17, line => 6},
                    start => #{character => 0, line => 6}
                },
            uri => ?config(implementation_a_uri, Config)
        },
        #{
            range =>
                #{
                    'end' => #{character => 17, line => 6},
                    start => #{character => 0, line => 6}
                },
            uri => ?config(implementation_b_uri, Config)
        }
    ],
    ?assertEqual(Expected, Result),
    ok.

-spec dynamic_call(config()) -> ok.
dynamic_call(Config) ->
    Uri = ?config(implementation_uri, Config),
    #{result := Result} = els_client:implementation(Uri, 6, 14),
    Expected = [
        #{
            range =>
                #{
                    'end' => #{character => 17, line => 6},
                    start => #{character => 0, line => 6}
                },
            uri => ?config(implementation_a_uri, Config)
        },
        #{
            range =>
                #{
                    'end' => #{character => 17, line => 6},
                    start => #{character => 0, line => 6}
                },
            uri => ?config(implementation_b_uri, Config)
        }
    ],
    ?assertEqual(Expected, Result),
    ok.
