-module(els_foldingrange_SUITE).

-include("erlang_ls.hrl").

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , groups/0
        , all/0
        ]).

%% Test cases
-export([ folding_range/1
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

-spec all() -> [{group, stdio | tcp}].
all() ->
  [{group, tcp}, {group, stdio}].

-spec groups() -> [atom()].
groups() ->
  els_test_utils:groups(?MODULE).

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

-spec folding_range(config()) -> ok.
folding_range(Config) ->
  #{result := Result} =
    els_client:folding_range(?config(code_navigation_uri, Config)),
  Expected = [ #{ endCharacter   => -1
                , endLine        => 23
                , startCharacter => 1
                , startLine      => 21
                }
             , #{ endCharacter   => -1
                , endLine        => 26
                , startCharacter => 1
                , startLine      => 25
                }
             , #{ endCharacter   => -1
                , endLine        => 29
                , startCharacter => 1
                , startLine      => 28
                }
             , #{ endCharacter    => -1
                , endLine        => 35
                , startCharacter => 1
                , startLine      => 31
                }
             , #{ endCharacter    => -1
                , endLine        => 40
                , startCharacter => 1
                , startLine      => 39
                }
             , #{ endCharacter    => -1
                , endLine        => 43
                , startCharacter => 1
                , startLine      => 42
                }
             , #{ endCharacter    => -1
                , endLine        => 48
                , startCharacter => 1
                , startLine      => 47
                }
             , #{ endCharacter    => -1
                , endLine        => 53
                , startCharacter => 1
                , startLine      => 50
                }
             , #{ endCharacter    => -1
                , endLine        => 57
                , startCharacter => 1
                , startLine      => 56
                }
             , #{ endCharacter    => -1
                , endLine        => 68
                , startCharacter => 1
                , startLine      => 67
                }
             , #{ endCharacter    => -1
                , endLine        => 76
                , startCharacter => 1
                , startLine      => 74
                }
             ],
  ?assertEqual(Expected, Result),
  ok.
