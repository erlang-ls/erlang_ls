-module(els_formatter_SUITE).

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
-export([format_doc/1]).

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
init_per_testcase(TestCase, Config) when
    TestCase == format_doc
->
    case els_utils:is_windows() of
        true ->
            %% TODO: Testcase fails on windows since OTP 24, fix!
            {skip, "Testcase not supported on Windows."};
        false ->
            els_test_utils:init_per_testcase(TestCase, Config)
    end;
init_per_testcase(TestCase, Config) ->
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec format_doc(config()) -> ok.
format_doc(Config) ->
    {ok, Cwd} = file:get_cwd(),
    RootPath = els_test_utils:root_path(),
    try
        file:set_cwd(RootPath),
        Uri = ?config(format_input_uri, Config),
        ok = els_config:set(formatting, #{}),
        #{result := Result} = els_client:document_formatting(Uri, 8, true),
        ?assertEqual(
            [
                #{
                    newText => <<"-spec main(any()) -> any().\n">>,
                    range =>
                        #{
                            'end' => #{character => 0, line => 5},
                            start => #{character => 0, line => 4}
                        }
                },
                #{
                    newText => <<"        X.\n">>,
                    range =>
                        #{
                            'end' => #{character => 0, line => 9},
                            start => #{character => 0, line => 6}
                        }
                }
            ],
            Result
        )
    after
        file:set_cwd(Cwd)
    end,
    ok.
