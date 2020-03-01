-module(els_completion_SUITE).

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
        , functions_export_list/1
        , handle_empty_lines/1
        , handle_colon_inside_string/1
        , macros/1
        , only_exported_functions_after_colon/1
        , records/1
        , record_fields/1
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
                , label            => <<"code_navigation_types">>
                }
             , #{ insertText => <<"do_4(${1:Arg1}, ${2:Arg2})">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION,
                  label => <<"do_4/2">>
                }
             , #{ insertText => <<"do_3(${1:Arg1}, ${2:Arg2})">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION,
                  label => <<"do_3/2">>
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
             ] ++ els_completion_provider:keywords(),
  #{ result := Completion
   } = els_client:completion(Uri, 9, 6, TriggerKind, <<"d">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion)),
  ok.

-spec empty_completions(config()) -> ok.
empty_completions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),

  #{ result := Completion
   } = els_client:completion(Uri, 5, 1, TriggerKind, <<"d">>),
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
    els_client:completion(Uri, 32, 25, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion1)),

  #{result := Completion2} =
    els_client:completion(Uri, 52, 34, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion2)),

  ok.

%% [#200] Complete only with arity for named funs
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
    els_client:completion(Uri, 52, 35, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

%% [#200] Complete only with arity for named funs
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
                      , <<"function_k/0">>
                      ],
  ExpectedCompletion = [ #{ label            => FunName
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                         || FunName <- ExportedFunctions
                       ],

  #{result := Completion} =
    els_client:completion(Uri, 51, 17, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

%% [#196] Complete only with arity for funs inside the export list
-spec functions_export_list(config()) -> ok.
functions_export_list(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"do/1">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                       , #{ label            => <<"do_2/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          }
                       ,  #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                           , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                           , label            => <<"do_3/2">>
                           }
                       , #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , label            => <<"do_4/2">>
                          }
                       ] ++ els_completion_provider:keywords(),

  #{result := Completion} =
    els_client:completion(Uri, 3, 13, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

-spec handle_empty_lines(config()) -> ok.
handle_empty_lines(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

  #{ result := Completion1
   } = els_client:completion(Uri, 32, 1, TriggerKind, <<"">>),
  ?assertEqual(null, Completion1),

  #{ result := Completion2
   } = els_client:completion(Uri, 32, 2, TriggerKind, <<":">>),
  ?assertEqual(null, Completion2),

  ok.

-spec handle_colon_inside_string(config()) -> ok.
handle_colon_inside_string(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

  #{ result := Completion
   } = els_client:completion(Uri, 76, 10, TriggerKind, <<":">>),
  ?assertEqual(null, Completion),

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
    els_client:completion(Uri, 24, 1, TriggerKindChar, <<"?">>),
  [?assert(lists:member(E, Completion1)) || E <- Expected],

  #{result := Completion2} =
    els_client:completion(Uri, 40, 5, TriggerKindInvoked, <<"">>),
  [?assert(lists:member(E, Completion2)) || E <- Expected],

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
    els_client:completion(Uri, 32, 26, TriggerKind, <<"d">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

-spec records(config()) -> ok.
records(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Expected = [ #{ kind => ?COMPLETION_ITEM_KIND_STRUCT
                , label => <<"included_record_a">>
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_STRUCT
                , label => <<"record_a">>
                }
             ],

  #{result := Completion1} =
    els_client:completion(Uri, 24, 1, TriggerKindChar, <<"#">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion1)),

  #{result := Completion2} =
    els_client:completion(Uri, 23, 6, TriggerKindInvoked, <<"">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion2)),

  ok.

-spec record_fields(config()) -> ok.
record_fields(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Expected1 = [ #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                 , label => <<"field_a">>
                 }
              , #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                 , label => <<"field_b">>
                 }
              ],
  #{result := Completion1} =
    els_client:completion(Uri, 34, 19, TriggerKindChar, <<".">>),
  ?assertEqual(lists:sort(Expected1), lists:sort(Completion1)),

  #{result := Completion2} =
    els_client:completion(Uri, 34, 22, TriggerKindInvoked, <<"">>),
  ?assertEqual(lists:sort(Expected1), lists:sort(Completion2)),

  Expected2 = [ #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                 , label => <<"included_field_a">>
                 }
              , #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                 , label => <<"included_field_b">>
                 }
              ],
  #{result := Completion3} =
    els_client:completion(Uri, 52, 60, TriggerKindChar, <<".">>),
  ?assertEqual(lists:sort(Expected2), lists:sort(Completion3)),

  #{result := Completion4} =
    els_client:completion(Uri, 52, 63, TriggerKindInvoked, <<"">>),
  ?assertEqual(lists:sort(Expected2), lists:sort(Completion4)),

  ok.

-spec variables(config()) -> ok.
variables(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  Expected = [ #{ kind => ?COMPLETION_ITEM_KIND_VARIABLE
                , label => <<"_Config">>
                }],

  #{result := Completion} =
    els_client:completion(Uri, 5, 8, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion)),

  ok.
