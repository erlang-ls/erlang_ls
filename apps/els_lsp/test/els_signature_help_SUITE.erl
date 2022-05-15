-module(els_signature_help_SUITE).
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
    remote_call/1,
    switch_signature_between_arities/1,
    non_trigger_character_request/1,
    argument_expressions_may_contain_commas/1,
    multiline_call/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
%% -include_lib("els_core/include/els_core.hrl").
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
-spec remote_call(config()) -> ok.
remote_call(Config) ->
    %% Line 6 of this document: "    erlang:min(A, B)."
    Uri = ?config(signature_help_uri, Config),
    %% On the "(" of "erlang:min("
    #{result := Result1} = els_client:signature_help(Uri, 6, 16),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 0,
            signatures := [
                #{
                    label := <<"min(A, B)">>,
                    parameters := [#{label := <<"A">>}, #{label := <<"B">>}],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                }
            ]
        },
        Result1
    ),
    %% On the "," of "erlang:min(A,"
    #{result := Result2} = els_client:signature_help(Uri, 6, 18),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 1,
            signatures := [#{label := <<"min(A, B)">>}]
        },
        Result2
    ),
    %% On the ")" of "erlang:min(A, B)"
    #{result := Result3} = els_client:signature_help(Uri, 6, 21),
    ?assertEqual(null, Result3),
    ok.

-spec switch_signature_between_arities(config()) -> ok.
switch_signature_between_arities(Config) ->
    %% Line 9 of this document: "    maps:get(key, #{}, false)."
    Uri = ?config(signature_help_uri, Config),
    %% On the "(" of "maps:get("
    #{result := Result1} = els_client:signature_help(Uri, 9, 14),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 0,
            signatures := [
                #{
                    label := <<"get(Arg1, Arg2)">>,
                    parameters := [#{label := <<"Arg1">>}, #{label := <<"Arg2">>}],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                },
                #{
                    label := <<"get(Key, Map, Default)">>,
                    parameters := [
                        #{label := <<"Key">>}, #{label := <<"Map">>}, #{label := <<"Default">>}
                    ],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                }
            ]
        },
        Result1
    ),
    %% On the "," of "maps:get(key,"
    #{result := Result2} = els_client:signature_help(Uri, 9, 18),
    ?assertMatch(#{activeSignature := 0, activeParameter := 1}, Result2),
    %% On the second "," of "maps:get(key, #{},", we switch to the higher
    %% arity `maps:get/3' signature
    #{result := Result3} = els_client:signature_help(Uri, 9, 23),
    ?assertMatch(#{activeSignature := 1, activeParameter := 2}, Result3),
    %% On the ")" of "maps:get(key, #{}, false)"
    #{result := Result4} = els_client:signature_help(Uri, 9, 30),
    ?assertEqual(null, Result4),
    ok.

-spec non_trigger_character_request(config()) -> ok.
non_trigger_character_request(Config) ->
    %% Line 9 of this document: "    maps:get(key, #{}, false)."
    Uri = ?config(signature_help_uri, Config),
    %% On the "e" of "maps:get(key"
    #{result := Result} = els_client:signature_help(Uri, 9, 16),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 0,
            signatures := [
                #{
                    label := <<"get(Arg1, Arg2)">>,
                    parameters := [#{label := <<"Arg1">>}, #{label := <<"Arg2">>}],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                }
                | _
            ]
        },
        Result
    ),
    ok.

-spec argument_expressions_may_contain_commas(config()) -> ok.
argument_expressions_may_contain_commas(Config) ->
    %% Line 12 of this document: "    erlang:max({a, b, c}, {d, e, f})."
    Uri = ?config(signature_help_uri, Config),
    %% On the "," of "erlang:max({a,"
    %% The comma belongs to the argument expression instead of separating
    %% arguments.
    #{result := Result1} = els_client:signature_help(Uri, 12, 19),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 0,
            signatures := [
                #{
                    label := <<"max(A, B)">>,
                    parameters := [#{label := <<"A">>}, #{label := <<"B">>}],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                }
            ]
        },
        Result1
    ),
    %% On the last "," of "erlang:max({a, b, c}, {d,"
    #{result := Result2} = els_client:signature_help(Uri, 12, 30),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 1,
            signatures := [#{label := <<"max(A, B)">>}]
        },
        Result2
    ),
    ok.

-spec multiline_call(config()) -> ok.
multiline_call(Config) ->
    %% The block being tested here is lines 15-18:
    %%
    %%    erlang:min(
    %%      1,
    %%      2
    %%    ).
    Uri = ?config(signature_help_uri, Config),
    %% On the "," on line 16
    #{result := Result} = els_client:signature_help(Uri, 16, 9),
    ?assertMatch(
        #{
            activeSignature := 0,
            activeParameter := 1,
            signatures := [
                #{
                    label := <<"min(A, B)">>,
                    parameters := [#{label := <<"A">>}, #{label := <<"B">>}],
                    documentation := #{
                        kind := <<"markdown">>
                    }
                }
            ]
        },
        Result
    ),
    ok.
