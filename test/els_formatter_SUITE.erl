-module(els_formatter_SUITE).

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
-export([ format_doc/1
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
-spec format_doc(config()) -> ok.
format_doc(Config) ->
  Cwd = file:get_cwd(),
  RootPath = ?config(root_path, Config),
  try
      file:set_cwd(RootPath),
      Uri = ?config(format_input_uri, Config),
      #{result := Result} = els_client:document_formatting(Uri, 8, true),
      ?assertEqual(
             [#{newText => <<"-spec main(any()) -> any().\n">>,
               range =>
                   #{'end' => #{character => 0,line => 7},
                     start => #{character => 0,line => 4}}},
             #{newText => <<"main(X) -> X.\n">>,
               range =>
                   #{'end' => #{character => 0,line => 8},
                     start => #{character => 0,line => 8}}}]
             , Result)
  after
      file:set_cwd(Cwd)
  end,
  ok.
