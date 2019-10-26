-module(erlang_ls_completion_SUITE).

-include("erlang_ls.hrl").

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ exported_functions/1
        , handle_empty_lines/1
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(erlang_ls),
  [{started, Started}|Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  erlang_ls_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok.

-spec all() -> [atom()].
all() ->
  erlang_ls_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec exported_functions(config()) -> ok.
exported_functions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"do/1">>
                          , insertText       => <<"do($1)">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       , #{ label            => <<"do_2/0">>
                          , insertText       => <<"do_2()">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       ],

  #{result := Completion1} =
    erlang_ls_client:completion(Uri, 32, 25, TriggerKind, <<":">>),
  ?assertEqual(Completion1, ExpectedCompletion),

  #{result := Completion2} =
    erlang_ls_client:completion(Uri, 52, 34, TriggerKind, <<":">>),
  ?assertEqual(Completion2, ExpectedCompletion),

  ok.

-spec handle_empty_lines(config()) -> ok.
handle_empty_lines(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

  #{ result := Completion1
   } = erlang_ls_client:completion(Uri, 32, 1, TriggerKind, <<"">>),
  ?assertEqual(null, Completion1),

  #{ result := Completion2
   } = erlang_ls_client:completion(Uri, 32, 2, TriggerKind, <<":">>),
  ?assertEqual(null, Completion2),

  ok.
