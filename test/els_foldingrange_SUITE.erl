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
  Expected = [ #{ endCharacter   => ?END_OF_LINE
                , endLine        => 22
                , startCharacter => ?END_OF_LINE
                , startLine      => 20
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 25
                , startCharacter => ?END_OF_LINE
                , startLine      => 24
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 28
                , startCharacter => ?END_OF_LINE
                , startLine      => 27
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 34
                , startCharacter => ?END_OF_LINE
                , startLine      => 30
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 39
                , startCharacter => ?END_OF_LINE
                , startLine      => 38
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 42
                , startCharacter => ?END_OF_LINE
                , startLine      => 41
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 47
                , startCharacter => ?END_OF_LINE
                , startLine      => 46
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 52
                , startCharacter => ?END_OF_LINE
                , startLine      => 49
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 56
                , startCharacter => ?END_OF_LINE
                , startLine      => 55
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 67
                , startCharacter => ?END_OF_LINE
                , startLine      => 66
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 75
                , startCharacter => ?END_OF_LINE
                , startLine      => 73
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 80
                , startCharacter => ?END_OF_LINE
                , startLine      => 78
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 85
                , startCharacter => ?END_OF_LINE
                , startLine      => 83
                }
             , #{ endCharacter   => ?END_OF_LINE
                , endLine        => 89
                , startCharacter => ?END_OF_LINE
                , startLine      => 88
                }
             ],
  ?assertEqual(Expected, Result),
  ok.
