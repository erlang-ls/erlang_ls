-module(els_completion_SUITE).

-include("els_lsp.hrl").

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
-export([ default_completions/1
        , empty_completions/1
        , exported_functions/1
        , exported_functions_arity/1
        , exported_types/1
        , functions_arity/1
        , functions_export_list/1
        , handle_empty_lines/1
        , handle_colon_inside_string/1
        , macros/1
        , only_exported_functions_after_colon/1
        , records/1
        , record_fields/1
        , types/1
        , types_export_list/1
        , variables/1
        , remote_fun/1
        , snippets/1
        , resolve_application_local/1
        , resolve_application_remote_self/1
        , resolve_application_remote_external/1
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

-spec all() -> [{group, atom()}].
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

-spec default_completions(config()) -> ok.
default_completions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  Functions = [ #{ insertText => <<"do_3(${1:Arg1}, ${2:Arg2})">>
                 , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                 , kind => ?COMPLETION_ITEM_KIND_FUNCTION
                 , label => <<"do_3/2">>
                 , data => #{ module => <<"code_navigation_extra">>
                            , function => <<"do_3">>
                            , arity => 2
                            }
                 }
              , #{ insertText => <<"do_2()">>
                 , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                 , kind => ?COMPLETION_ITEM_KIND_FUNCTION
                 , label => <<"do_2/0">>
                 , data => #{ module => <<"code_navigation_extra">>
                            , function => <<"do_2">>
                            , arity => 0
                            }
                 }
              , #{ insertText => <<"do(${1:_Config})">>
                 , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                 , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                 , label => <<"do/1">>
                 , data => #{ module => <<"code_navigation_extra">>
                            , function => <<"do">>
                            , arity => 1
                            }
                 }
              , #{ insertText => <<"do_4(${1:Arg1}, ${2:Arg2})">>
                 , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                 , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                 , label => <<"do_4/2">>
                 , data => #{ module => <<"code_navigation_extra">>
                            , function => <<"do_4">>
                            , arity => 2
                            }
                 }
              , #{ insertText => <<"'DO_LOUDER'()">>
                 , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                 , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                 , label => <<"'DO_LOUDER'/0">>
                 , data => #{ module => <<"code_navigation_extra">>
                            , function => <<"DO_LOUDER">>
                            , arity => 0
                            }
                 }
              ],

  Expected1 = [ #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                 , kind             => ?COMPLETION_ITEM_KIND_MODULE
                 , label            => <<"code_lens_function_references">>
                 }
              , #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
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
              | Functions ],

  DefaultCompletion = els_completion_provider:keywords()
                        ++ els_completion_provider:bifs(function, false)
                        ++ els_snippets_server:snippets(),
  #{ result := Completion1
   } = els_client:completion(Uri, 9, 6, TriggerKind, <<"">>),
  ?assertEqual(
    lists:sort(Expected1),
    filter_completion(Completion1, DefaultCompletion)),

  Expected2 = [ #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                 , kind             => ?COMPLETION_ITEM_KIND_CONSTANT
                 , label            => <<"foo">>
                 }
              | Functions ],

  #{ result := Completion2
   } = els_client:completion(Uri, 6, 14, TriggerKind, <<"">>),
  ?assertEqual(
    lists:sort(Expected2),
    filter_completion(Completion2, DefaultCompletion)),

  ok.

-spec empty_completions(config()) -> ok.
empty_completions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),

  #{ result := Completion
   } = els_client:completion(Uri, 5, 1, TriggerKind, <<"">>),
  ?assertEqual([], Completion),
  ok.

-spec exported_functions(config()) -> ok.
exported_functions(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion1 = expected_exported_functions(),

  #{result := Completion1} =
    els_client:completion(Uri, 32, 25, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(ExpectedCompletion1), lists:sort(Completion1)),

  #{result := Completion2} =
    els_client:completion(Uri, 52, 34, TriggerKind, <<":">>),
  ExpectedCompletionArity = expected_exported_functions_arity_only(),
  ?assertEqual(lists:sort(ExpectedCompletionArity), lists:sort(Completion2)),

  ExpectedCompletionQuoted =
    [ #{ label            => <<"do/1">>
       , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
       , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
       , data             =>
           #{ module => <<"Code.Navigation.Elixirish">>
            , function => <<"do">>
            , arity => 1
            }
       }
    ],
  #{result := Completion3} =
    els_client:completion(Uri, 100, 39, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(ExpectedCompletionQuoted), lists:sort(Completion3)),

  ok.

%% [#200] Complete only with arity for named funs
-spec exported_functions_arity(config()) -> ok.
exported_functions_arity(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  ExpectedCompletion = expected_exported_functions_arity_only(),
  #{result := Completion} =
    els_client:completion(Uri, 52, 35, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

-spec exported_types(config()) -> ok.
exported_types(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  Uri = ?config(code_navigation_uri, Config),
  Types = [ <<"date_time">>, <<"fd">>, <<"file_info">>, <<"filename">>
          , <<"filename_all">>, <<"io_device">>, <<"mode">>, <<"name">>
          , <<"name_all">>, <<"posix">>
          ],
  Expected = [ #{ insertText => <<T/binary, "()">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM
                , label => <<T/binary, "/0">>
                , data => #{}
                }
               || T <- Types
             ],

  ct:comment("Exported types from module are returned in a spec context"),
  #{result := Completion1} =
    els_client:completion(Uri, 55, 60, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(Expected), lists:sort(Completion1)),

  ok.

%% [#200] Complete only with arity for named funs
-spec functions_arity(config()) -> ok.
functions_arity(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  ExportedFunctions = [ {<<"callback_a">>, 0}
                      , {<<"function_a">>, 0}
                      , {<<"function_b">>, 0}
                      , {<<"function_c">>, 0}
                      , {<<"function_d">>, 0}
                      , {<<"function_e">>, 0}
                      , {<<"function_f">>, 0}
                      , {<<"function_g">>, 1}
                      , {<<"function_h">>, 0}
                      , {<<"function_i">>, 0}
                      , {<<"function_j">>, 0}
                      , {<<"function_k">>, 0}
                      , {<<"function_l">>, 2}
                      , {<<"function_m">>, 1}
                      , {<<"function_n">>, 0}
                      , {<<"function_o">>, 0}
                      , {<<"'PascalCaseFunction'">>, 1}
                      , {<<"function_p">>, 1}
                      ],
  ExpectedCompletion =
    [ #{ label =>
           <<FunName/binary, "/", (integer_to_binary(Arity))/binary>>
       , kind => ?COMPLETION_ITEM_KIND_FUNCTION
       , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
       , data => #{ module => <<"code_navigation">>
                  , function => string:trim(FunName, both, [$'])
                  , arity => Arity
                  }
       }
      || {FunName, Arity} <- ExportedFunctions
    ] ++ els_completion_provider:bifs(function, true),
  #{result := Completion} =
    els_client:completion(Uri, 51, 17, TriggerKind, <<"">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

%% [#196] Complete only with arity for funs inside the export list
-spec functions_export_list(config()) -> ok.
functions_export_list(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_extra_uri, Config),
  Expected = [ #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                , label            => <<"do_3/2">>
                , data             =>
                    #{ module => <<"code_navigation_extra">>
                     , function => <<"do_3">>
                     , arity => 2
                     }
                }
             , #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                , label            => <<"do_4/2">>
                , data             =>
                    #{ module => <<"code_navigation_extra">>
                     , function => <<"do_4">>
                     , arity => 2
                     }
                }
             ],
  #{result := Completion} =
    els_client:completion(Uri, 3, 13, TriggerKind, <<"">>),
  ?assertEqual(Expected, Completion).

-spec handle_empty_lines(config()) -> ok.
handle_empty_lines(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

  #{ result := Completion1
   } = els_client:completion(Uri, 32, 1, TriggerKind, <<"">>),
  ?assertEqual([], Completion1),

  #{ result := Completion2
   } = els_client:completion(Uri, 32, 2, TriggerKind, <<":">>),
  ?assertEqual([], Completion2),

  ok.

-spec handle_colon_inside_string(config()) -> ok.
handle_colon_inside_string(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

  #{ result := Completion
   } = els_client:completion(Uri, 76, 10, TriggerKind, <<":">>),
  ?assertEqual([], Completion),

  ok.

-spec macros(config()) -> ok.
macros(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Expected = [ #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"INCLUDED_MACRO_A">>
                , data => #{}
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"MACRO_A">>
                , data => #{}
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"MACRO_WITH_ARGS">>
                , data => #{}
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_CONSTANT
                , label => <<"macro_A">>
                , data => #{}
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
  ExpectedCompletion = expected_exported_functions(),

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
                , data => #{}
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_STRUCT
                , label => <<"record_a">>
                , data => #{}
                }
             , #{ kind => ?COMPLETION_ITEM_KIND_STRUCT
                , label => <<"'PascalCaseRecord'">>
                , data => #{}
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
              , #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                 , label => <<"'Field C'">>
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

  ExpectedQuoted = [ #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                      , label => <<"'Field #1'">>
                      }
                   , #{ kind => ?COMPLETION_ITEM_KIND_FIELD
                      , label => <<"'Field #2'">>
                      }
                   ],
  #{result := Completion5} =
    els_client:completion(Uri, 52, 90, TriggerKindChar, <<".">>),
  ?assertEqual(lists:sort(ExpectedQuoted), lists:sort(Completion5)),

  ok.

-spec types(config()) -> ok.
types(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_uri, Config),
  Expected = [ #{ insertText       => <<"type_a()">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_TYPE_PARAM
                , label            => <<"type_a/0">>
                , data             => #{}
                }
             , #{ insertText       => <<"included_type_a()">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_TYPE_PARAM
                , label            => <<"included_type_a/0">>
                , data             => #{}
                }
             , #{ insertText       => <<"'INCLUDED_TYPE'(${1:T})">>
                , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
                , kind             => ?COMPLETION_ITEM_KIND_TYPE_PARAM
                , label            => <<"'INCLUDED_TYPE'/1">>
                , data             => #{}
                }
             ],

  DefaultCompletion = els_completion_provider:keywords()
                        ++ els_completion_provider:bifs(type_definition, false)
                        ++ els_snippets_server:snippets(),

  ct:comment("Types defined both in the current file and in includes"),
  #{result := Completion1} =
    els_client:completion(Uri, 55, 27, TriggerKind, <<"">>),
  ?assertEqual(
    lists:sort(Expected),
    filter_completion(Completion1, DefaultCompletion)),

  ok.

-spec types_export_list(config()) -> ok.
types_export_list(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(code_navigation_types_uri, Config),
  Expected = [ #{ insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                , kind             => ?COMPLETION_ITEM_KIND_TYPE_PARAM
                , label            => <<"user_type_a/0">>
                , data             => #{}
                }
             ],
  ct:comment("Types in an export_type section is provided with arity"),
  #{result := Completion} =
    els_client:completion(Uri, 5, 19, TriggerKind, <<"">>),
  ?assertEqual(Expected, Completion),
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

expected_exported_functions() ->
  [ #{ label            => <<"do/1">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertText       => <<"do(${1:_Config})">>
     , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"do">>
                            , arity => 1
                            }
     }
  , #{ label            => <<"do_2/0">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertText       => <<"do_2()">>
     , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"do_2">>
                            , arity => 0
                            }
     }
  , #{ label            => <<"'DO_LOUDER'/0">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertText       => <<"'DO_LOUDER'()">>
     , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"DO_LOUDER">>
                            , arity => 0
                            }
     }
  ].

expected_exported_functions_arity_only() ->
  [ #{ label            => <<"do/1">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"do">>
                            , arity => 1
                            }
     }
  , #{ label            => <<"do_2/0">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"do_2">>
                            , arity => 0
                            }
     }
  , #{ label            => <<"'DO_LOUDER'/0">>
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
     , data             => #{ module => <<"code_navigation_extra">>
                            , function => <<"DO_LOUDER">>
                            , arity => 0
                            }
     }
  ].

%% [#790] Complete only with arity for remote applications
-spec remote_fun(config()) -> ok.
remote_fun(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
  Uri = ?config(completion_caller_uri, Config),
  ExpectedCompletion = [ #{ label            => <<"complete_1/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          , data             => #{ module => <<"completion">>
                                                 , function => <<"complete_1">>
                                                 , arity => 0
                                                 }
                          }
                       , #{ label            => <<"complete_2/0">>
                          , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
                          , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
                          , data             => #{ module => <<"completion">>
                                                 , function => <<"complete_2">>
                                                 , arity => 0
                                                 }
                          }
                       ],
  #{result := Completion} =
    els_client:completion(Uri, 6, 19, TriggerKind, <<":">>),
  ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

  ok.

-spec snippets(config()) -> ok.
snippets(Config) ->
  TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  Uri = ?config(completion_snippets_uri, Config),
  #{result := Result} = els_client:completion(Uri, 3, 6, TriggerKind, <<"">>),
  Completions = [C || #{kind := ?COMPLETION_ITEM_KIND_SNIPPET} = C <- Result],
  SnippetsDir = els_snippets_server:builtin_snippets_dir(),
  Snippets = filelib:wildcard("*", SnippetsDir),
  CustomSnippetsDir = els_snippets_server:custom_snippets_dir(),
  CustomSnippets = filelib:wildcard("*", CustomSnippetsDir),
  Expected = lists:usort(Snippets ++ CustomSnippets),
  ?assertEqual(length(Expected), length(Completions)).

filter_completion(Completion, ToFilter) ->
  CompletionSet = ordsets:from_list(Completion),
  FilterSet = ordsets:from_list(ToFilter),
  ?assertEqual(FilterSet, ordsets:intersection(CompletionSet, FilterSet)),
  ordsets:to_list(ordsets:subtract(CompletionSet, FilterSet)).

-spec resolve_application_local(config()) -> ok.
resolve_application_local(Config) ->
  Uri = ?config(completion_resolve_uri, Config),
  CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  #{result := CompletionItems} =
    els_client:completion(Uri, 17, 5, CompletionKind, <<"">>),
  [Selected] = select_completionitems(CompletionItems),
  #{result := Result} = els_client:completionitem_resolve(Selected),
  Expected = Selected#{ documentation =>
                          #{ kind => <<"markdown">>
                           , value =>
                               <<"## completion_resolve:call_1/0\n\n"
                                 "```erlang\n"
                                 "-spec call_1() -> 'ok'.\n"
                                 "```">>
                           }
                      },
  ?assertEqual(Expected, Result).

-spec resolve_application_remote_self(config()) -> ok.
resolve_application_remote_self(Config) ->
  Uri = ?config(completion_resolve_uri, Config),
  CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  #{result := CompletionItems} =
    els_client:completion(Uri, 16, 23, CompletionKind, <<":">>),
  [Selected] = select_completionitems(CompletionItems),
  #{result := Result} = els_client:completionitem_resolve(Selected),
  Expected = Selected#{ documentation =>
                          #{ kind => <<"markdown">>
                           , value =>
                               <<"## completion_resolve:call_1/0\n\n"
                                 "```erlang\n"
                                 "-spec call_1() -> 'ok'.\n"
                                 "```">>
                           }
                      },
  ?assertEqual(Expected, Result).

-spec resolve_application_remote_external(config()) -> ok.
resolve_application_remote_external(Config) ->
  Uri = ?config(completion_resolve_uri, Config),
  CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
  #{result := CompletionItems} =
    els_client:completion(Uri, 18, 25, CompletionKind, <<":">>),
  [Selected] = select_completionitems(CompletionItems),
  #{result := Result} = els_client:completionitem_resolve(Selected),
  Expected = Selected#{ documentation =>
                          #{ kind => <<"markdown">>
                           , value =>
                               <<"## completion_resolve_2:call_1/0\n\n"
                                 "```erlang\n"
                                 "-spec call_1() -> 'ok'.\n"
                                 "```">>
                           }
                      },
  ?assertEqual(Expected, Result).

select_completionitems(CompletionItems) ->
  [CI || #{ kind := ?COMPLETION_ITEM_KIND_FUNCTION
          , label := <<"call_1/0">>
          } = CI <- CompletionItems].
