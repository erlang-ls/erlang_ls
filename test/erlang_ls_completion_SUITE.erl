-module(erlang_ls_completion_SUITE).

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
-export([ atom_completions/1
        , empty_completions/1
        , exported_functions/1
        , exported_functions_arity/1
        , functions_arity/1
        , handle_empty_lines/1
        , macros/1
        , only_exported_functions_after_colon/1
        , variables/1
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
  erlang_ls_test_utils:groups(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  erlang_ls_test_utils:init_per_suite(Config).

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  erlang_ls_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  erlang_ls_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) ->
  erlang_ls_test_utils:end_per_testcase(TestCase, Config).

%%==============================================================================
%% Testcases
%%==============================================================================

-spec atom_completions(config()) -> ok.
atom_completions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  Expected = [ #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_MODULE
                , label            => <<"code_navigation_extra">>
                }
             , #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_MODULE
                , label            => <<"code_navigation">>
                }
             , #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_MODULE
                , label            => <<"code_navigation.hrl">>
                }
             , #{ insertText => <<"do_3()">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION,
                  label => <<"do_3/0">>
                }
             , #{ insertText => <<"do_2()">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION,
                  label => <<"do_2/0">>
                }
             , #{ insertText => <<"do(${1:_Config})">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION,
                  label => <<"do/1">>
                }
             ] ++ erlang_ls_completion_provider:keywords(),
  #{ result := Completion
   } = erlang_ls_client:completion(Uri, 9, 6, TriggerKind, <<"d">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion)),
  ok.

-spec empty_completions(config()) -> ok.
empty_completions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),

  #{ result := Completion
   } = erlang_ls_client:completion(Uri, 5, 1, TriggerKind, <<"d">>),
  ?assertEqual([], Completion),
  ok.

-spec exported_functions(config()) -> ok.
exported_functions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"do/1">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertText       => <<"do(${1:_Config})">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       , #{ label            => <<"do_2/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertText       => <<"do_2()">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       ],

  #{result := Completion1} =
    erlang_ls_client:completion(Uri, 32, 25, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(Completion1), lists:sort(ExpectedCompletion)),

  #{result := Completion2} =
    erlang_ls_client:completion(Uri, 52, 34, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(Completion2), lists:sort(ExpectedCompletion)),

  ok.

-spec exported_functions_arity(config()) -> ok.
exported_functions_arity(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"do/1">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                       , #{ label            => <<"do_2/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                       ],

  #{result := Completion} =
    erlang_ls_client:completion(Uri, 52, 35, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(Completion), lists:sort(ExpectedCompletion)),

  ok.

-spec functions_arity(config()) -> ok.
functions_arity(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  ExportedFunctions = [ <<"callback_a/0">>
                      , <<"function_a/0">>
                      , <<"function_b/0">>
                      , <<"function_c/0">>
                      , <<"function_d/0">>
                      , <<"function_e/0">>
                      , <<"function_f/0">>
                      , <<"function_g/1">>
                      , <<"function_h/0">>
                      , <<"function_i/0">>
                      , <<"function_j/0">>
                      ],
  ExpectedCompletion = [ #{ label            => FunName
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                         || FunName <- ExportedFunctions
                       ],

  #{result := Completion} =
    erlang_ls_client:completion(Uri, 51, 17, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

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

-spec macros(config()) -> ok.
macros(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Expected = [ #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"INCLUDED_MACRO_A">>
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"MACRO_A">>
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"MACRO_WITH_ARGS">>
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"macro_A">>
                }
             ],

  #{result := Completion1} =
    erlang_ls_client:completion(Uri, 24, 1, TriggerKindChar, <<"?">>),
  ?assertEqual(lists:sort(Completion1), lists:sort(Expected)),

  #{result := Completion2} =
    erlang_ls_client:completion(Uri, 40, 5, TriggerKindInvoked, <<"">>),
  ?assertEqual(lists:sort(Completion2), lists:sort(Expected)),

  ok.

-spec only_exported_functions_after_colon(config()) -> ok.
only_exported_functions_after_colon(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"do/1">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertText       => <<"do(${1:_Config})">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       , #{ label            => <<"do_2/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertText       => <<"do_2()">>
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                          }
                       ],

  #{result := Completion} =
    erlang_ls_client:completion(Uri, 32, 26, TriggerKind, <<"d">>),
  ?assertEqual(lists:sort(Completion), lists:sort(ExpectedCompletion)),

  ok.

-spec variables(config()) -> ok.
variables(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  Expected = [ #{ kind => ?COMPLETION_ITEM_KIND_VARIABLE
                , label => <<"_Config">>
                }],

  #{result := Completion} =
    erlang_ls_client:completion(Uri, 5, 8, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(Completion), lists:sort(Expected)),

  ok.
