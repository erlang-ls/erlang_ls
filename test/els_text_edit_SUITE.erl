-module(els_text_edit_SUITE).

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
-export([ text_edit_diff/1
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
-spec text_edit_diff(config()) -> ok.
text_edit_diff(Config) ->
  DiagnosticsUri = ?config(diagnostics_uri, Config),
  DiagnosticsPath = els_uri:path(DiagnosticsUri),
  DiagnosticsDiffPath = ?config('diagnostics.new_path', Config),
  Result = els_text_edit:diff_files(DiagnosticsPath, DiagnosticsDiffPath),
  [Edit1, Edit2] = Result,
  ?assertEqual( #{newText =>
                   <<"%% Changed diagnostics.erl, to test diff generation\n">>,
                  range =>
                      #{'end' => #{character => 0, line => 1},
                        start => #{character => 0, line => 1}}}
              , Edit1),
  ?assertEqual( #{newText => <<"main(X) -> X + 1.\n">>,
                  range =>
                      #{'end' => #{character => 0, line => 8},
                        start => #{character => 0, line => 6}}}
              , Edit2),
  ok.
