-module(els_io_string_SUITE).

%% CT Callbacks
-export([ init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ scan_forms/1
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
-spec all() -> [atom()].
all() -> els_test_utils:all(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  els_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) -> Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) -> ok.

%%==============================================================================
%% Testcases
%%==============================================================================

-spec scan_forms(config()) -> ok.
scan_forms(Config) ->
  Path         = path(Config),
  {ok, IoFile} = file:open(Path, [read]),
  Expected     = scan_all_forms(IoFile, []),
  ok           = file:close(IoFile),

  {ok, Text} = file:read_file(Path),
  IoString   = els_io_string:new(Text),
  Result     = scan_all_forms(IoString, []),
  ok         = file:close(IoString),

  ?assertEqual(Expected, Result),

  ok.

%%==============================================================================
%% Helper functions
%%==============================================================================

-spec scan_all_forms(file:io_device(), [any()]) -> [any()].
scan_all_forms(IoDevice, Acc) ->
  case io:scan_erl_form(IoDevice, "") of
    {ok, Tokens, _} ->
      scan_all_forms(IoDevice, [Tokens | Acc]);
    {eof, _} ->
      Acc
  end.

-spec path(config()) -> string().
path(Config) ->
  RootPath = ?config(root_path, Config) ,
  filename:join([RootPath, "src", "code_navigation.erl"]).
