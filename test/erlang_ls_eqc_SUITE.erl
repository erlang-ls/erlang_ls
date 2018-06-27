%%==============================================================================
%% Wrapper Suite for the QuickCheck tests
%%==============================================================================
-module(erlang_ls_eqc_SUITE).

-ifdef(EQC_TESTING).

%% Common Test Callbacks
-export([ all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Testcases
-export([ eqc/1 ]).

%%==============================================================================
%% Include
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% Common Test Callbacks
%%==============================================================================
-spec all() -> [atom()].
all() ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info],
  Exports = ?MODULE:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> any().
init_per_testcase(_TestCase, Config) ->
  Config.

-spec end_per_testcase(atom(), config()) -> any().
end_per_testcase(_TestCase, Config) ->
  Config.

%%==============================================================================
%% Test cases
%%==============================================================================
-spec eqc(config()) -> ok.
eqc(_Config) ->
  Property = erlang_ls_eqc:prop_main(),
  ?assert(eqc:quickcheck(eqc_statem:show_states(Property))).

-endif.
