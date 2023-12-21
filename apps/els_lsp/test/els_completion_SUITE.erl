-module(els_completion_SUITE).
%% CT Callbacks
-export([
    suite/0,
    init_per_suite/1,
    end_per_suite/1,
    init_per_testcase/2,
    end_per_testcase/2,
    all/0
]).

%% Test cases
-export([
    attributes/1,
    attribute_behaviour/1,
    attribute_include/1,
    attribute_include_lib/1,
    attribute_export/1,
    attribute_export_incomplete/1,
    attribute_export_type/1,
    default_completions/1,
    empty_completions/1,
    exported_functions/1,
    exported_functions_arity/1,
    exported_types/1,
    functions_arity/1,
    functions_export_list/1,
    functions_no_args/1,
    handle_empty_lines/1,
    handle_colon_inside_string/1,
    macros/1,
    only_exported_functions_after_colon/1,
    records/1,
    record_fields/1,
    record_fields_inside_record/1,
    types/1,
    types_export_list/1,
    types_context/1,
    types_no_args/1,
    variables/1,
    remote_fun/1,
    snippets/1,
    resolve_application_local/1,
    resolve_opaque_application_local/1,
    resolve_application_unexported_local/1,
    resolve_application_remote_self/1,
    resolve_application_remote_external/1,
    resolve_application_remote_otp/1,
    resolve_type_application_local/1,
    resolve_opaque_application_remote_self/1,
    resolve_type_application_remote_external/1,
    resolve_opaque_application_remote_external/1,
    resolve_type_application_remote_otp/1,
    completion_request_fails/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("els_core/include/els_core.hrl").
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
    els_test_utils:all(?MODULE).

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
-spec attributes(config()) -> ok.
attributes(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    Expected = [
        #{
            insertText => <<"behaviour(${1:Behaviour}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-behaviour().">>
        },
        #{
            insertText => <<"define(${1:MACRO}, ${2:Value}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-define().">>
        },
        #{
            insertText => <<"export([${1:}]).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-export().">>
        },
        #{
            insertText => <<"export_type([${1:}]).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-export_type().">>
        },
        #{
            insertText => <<"feature(${1:Feature}, ${2:enable}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-feature().">>
        },
        #{
            insertText => <<"if(${1:Pred}).\n${2:}\n-endif.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-if().">>
        },
        #{
            insertText => <<"ifdef(${1:VAR}).\n${2:}\n-endif.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-ifdef().">>
        },
        #{
            insertText => <<"ifndef(${1:VAR}).\n${2:}\n-endif.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-ifndef().">>
        },
        #{
            insertText => <<"include(${1:}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-include().">>
        },
        #{
            insertText => <<"include_lib(${1:}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-include_lib().">>
        },
        #{
            insertText => <<"opaque ${1:name}() :: ${2:definition}.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-opaque name() :: definition.">>
        },
        #{
            insertText => <<
                "record(${1:name}, {${2:field} = ${3:Value} "
                ":: ${4:Type}()})."
            >>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-record().">>
        },
        #{
            insertText => <<"type ${1:name}() :: ${2:definition}.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-type name() :: definition.">>
        },
        #{
            insertText => <<"dialyzer(${1:}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-dialyzer().">>
        },
        #{
            insertText => <<"compile(${1:}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-compile().">>
        },
        #{
            insertText => <<"import(${1:Module}, [${2:}]).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-import().">>
        },
        #{
            insertText =>
                <<"callback ${1:name}(${2:Args}) -> ${3:return()}.">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-callback name(Args) -> return().">>
        },
        #{
            insertText => <<"on_load(${1:Function}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-on_load().">>
        },
        #{
            insertText => <<"vsn(${1:Version}).">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_SNIPPET,
            label => <<"-vsn(Version).">>
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 5, 2, TriggerKindChar, <<"-">>),
    ?assertEqual([], Completions -- Expected),
    ?assertEqual([], Expected -- Completions),
    ok.

-spec attribute_behaviour(config()) -> ok.
attribute_behaviour(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"gen_event">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"gen_server">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"gen_statem">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"supervisor">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"behaviour_a">>
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 2, 12, TriggerKindInvoked, <<"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    ok.

-spec attribute_include(config()) -> ok.
attribute_include(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FILE,
            label => <<"code_navigation.hrl">>
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 3, 11, TriggerKindInvoked, <<"\"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    ok.

-spec attribute_include_lib(config()) -> ok.
attribute_include_lib(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FILE,
            label => <<"code_navigation/include/rename.hrl">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FILE,
            label => <<"code_navigation/include/code_navigation.hrl">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FILE,
            label => <<"code_navigation/include/diagnostics.hrl">>
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 4, 15, TriggerKindInvoked, <<"\"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    ok.

-spec attribute_export(config()) -> ok.
attribute_export(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"unexported_function/0">>,
            data =>
                #{
                    arity => 0,
                    function => <<"unexported_function">>,
                    module => <<"completion_attributes">>
                }
        }
    ],
    NotExpected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"exported_function/0">>,
            data =>
                #{
                    arity => 0,
                    function => <<"exported_function">>,
                    module => <<"completion_attributes">>
                }
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 5, 10, TriggerKindInvoked, <<"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    [?assertNot(lists:member(E, Completions)) || E <- NotExpected],
    ok.

-spec attribute_export_incomplete(config()) -> ok.
attribute_export_incomplete(Config) ->
    Uri = ?config(completion_incomplete_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"function_unexported/0">>,
            data =>
                #{
                    arity => 0,
                    function => <<"function_unexported">>,
                    module => <<"completion_incomplete">>
                }
        }
    ],
    NotExpected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"function_exported/0">>,
            data =>
                #{
                    arity => 0,
                    function => <<"function_exported">>,
                    module => <<"completion_incomplete">>
                }
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 4, 18, TriggerKindInvoked, <<"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    [?assertNot(lists:member(E, Completions)) || E <- NotExpected],
    ok.

-spec attribute_export_type(config()) -> ok.
attribute_export_type(Config) ->
    Uri = ?config(completion_attributes_uri, Config),
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"unexported_type/0">>,
            data => #{
                module => <<"completion_attributes">>,
                type => <<"unexported_type">>,
                arity => 0
            }
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"unexported_opaque/0">>,
            data => #{
                module => <<"completion_attributes">>,
                type => <<"unexported_opaque">>,
                arity => 0
            }
        }
    ],
    NotExpected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"exported_type/0">>,
            data => #{}
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"exported_opaque/0">>,
            data => #{}
        }
    ],
    #{result := Completions} =
        els_client:completion(Uri, 6, 15, TriggerKindInvoked, <<"">>),
    [?assert(lists:member(E, Completions)) || E <- Expected],
    [?assertNot(lists:member(E, Completions)) || E <- NotExpected],
    ok.

-spec default_completions(config()) -> ok.
default_completions(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_extra_uri, Config),
    Functions = [
        #{
            insertText => <<"do_3(${1:Arg1}, ${2:Arg2})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do_3/2">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do_3">>,
                arity => 2
            }
        },
        #{
            insertText => <<"do_2()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do_2/0">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do_2">>,
                arity => 0
            }
        },
        #{
            insertText => <<"do(${1:_Config})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do/1">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do">>,
                arity => 1
            }
        },
        #{
            insertText => <<"do_4(${1:Arg1}, ${2:Arg2})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do_4/2">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do_4">>,
                arity => 2
            }
        },
        #{
            insertText => <<"'DO_LOUDER'()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"'DO_LOUDER'/0">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"DO_LOUDER">>,
                arity => 0
            }
        },
        #{
            insertText => <<"function_a(${1:Arg1}, ${2:Arg2})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"function_a/2">>,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"function_a">>,
                arity => 2
            }
        }
    ],

    Expected1 = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_lens_function_references">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_navigation_extra">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_navigation">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"code_navigation">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_navigation_types">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_navigation_undefined">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_navigation_broken">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_action">>
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_MODULE,
            label => <<"code_completion_fail">>
        }
        | Functions
    ],
    DefaultCompletion =
        keywords() ++
            els_completion_provider:bifs(function, args) ++
            els_snippets_server:snippets(),
    #{result := Completion1} = els_client:completion(Uri, 9, 6, TriggerKind, <<"">>),
    ?assertEqual(
        [],
        filter_completion(Completion1, DefaultCompletion) -- Expected1
    ),
    ?assertEqual(
        [],
        Expected1 -- filter_completion(Completion1, DefaultCompletion)
    ),

    Expected2 = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"foo">>
        }
        | Functions
    ],

    #{result := Completion2} = els_client:completion(Uri, 6, 14, TriggerKind, <<"">>),
    ?assertEqual(
        lists:sort(Expected2),
        filter_completion(Completion2, DefaultCompletion)
    ),

    ok.

-spec empty_completions(config()) -> ok.
empty_completions(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_extra_uri, Config),

    #{result := Completion} = els_client:completion(Uri, 5, 1, TriggerKind, <<"">>),
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
        [
            #{
                label => <<"do/1">>,
                kind => ?COMPLETION_ITEM_KIND_FUNCTION,
                insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
                data =>
                    #{
                        module => <<"Code.Navigation.Elixirish">>,
                        function => <<"do">>,
                        arity => 1
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
    Types = file_exported_types(),
    Expected = [
        #{
            insertText => <<T/binary, "()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<T/binary, "/0">>,
            data => #{
                module => <<"file">>,
                type => T,
                arity => 0
            }
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
    ExportedFunctions = [
        {<<"callback_a">>, 0},
        {<<"function_a">>, 0},
        {<<"function_b">>, 0},
        {<<"function_c">>, 0},
        {<<"function_d">>, 0},
        {<<"function_e">>, 0},
        {<<"function_f">>, 0},
        {<<"function_g">>, 1},
        {<<"function_h">>, 0},
        {<<"function_i">>, 0},
        {<<"function_j">>, 0},
        {<<"function_k">>, 0},
        {<<"function_l">>, 2},
        {<<"function_m">>, 1},
        {<<"function_n">>, 0},
        {<<"function_o">>, 0},
        {<<"'PascalCaseFunction'">>, 1},
        {<<"function_p">>, 1},
        {<<"function_q">>, 0},
        {<<"macro_b">>, 2},
        {<<"function_mb">>, 0},
        {<<"code_navigation">>, 0},
        {<<"code_navigation">>, 1},
        {<<"multiple_instances_same_file">>, 0},
        {<<"code_navigation_extra">>, 3},
        {<<"multiple_instances_diff_file">>, 0}
    ],
    ExpectedCompletion =
        [
            #{
                label =>
                    <<FunName/binary, "/", (integer_to_binary(Arity))/binary>>,
                kind => ?COMPLETION_ITEM_KIND_FUNCTION,
                insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
                data => #{
                    module => <<"code_navigation">>,
                    function => string:trim(FunName, both, [$']),
                    arity => Arity
                }
            }
         || {FunName, Arity} <- ExportedFunctions
        ] ++ els_completion_provider:bifs(function, arity_only),
    #{result := Completion} =
        els_client:completion(Uri, 51, 17, TriggerKind, <<"">>),
    ?assertEqual(lists:sort(ExpectedCompletion), lists:sort(Completion)),

    ok.

%% [#196] Complete only with arity for funs inside the export list
-spec functions_export_list(config()) -> ok.
functions_export_list(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_extra_uri, Config),
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do_3/2">>,
            data =>
                #{
                    module => <<"code_navigation_extra">>,
                    function => <<"do_3">>,
                    arity => 2
                }
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            label => <<"do_4/2">>,
            data =>
                #{
                    module => <<"code_navigation_extra">>,
                    function => <<"do_4">>,
                    arity => 2
                }
        }
    ],
    #{result := Completion} =
        els_client:completion(Uri, 3, 13, TriggerKind, <<"">>),
    ?assertEqual(Expected, Completion).

-spec functions_no_args(config()) -> ok.
functions_no_args(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_extra_uri, Config),
    ct:comment("Completing function at func|t(), should include args"),
    #{result := Completion1} =
        els_client:completion(Uri, 26, 7, TriggerKind, <<>>),
    ?assertEqual(
        [<<"function_a(${1:Arg1}, ${2:Arg2})">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_FUNCTION,
                label := <<"function_a/2">>,
                insertText := InsertText
            } <- Completion1
        ]
    ),
    ct:comment("Completing function at funct|(), shouldn't include args"),
    #{result := Completion2} =
        els_client:completion(Uri, 26, 8, TriggerKind, <<>>),
    ?assertEqual(
        [<<"function_a">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_FUNCTION,
                label := <<"function_a/2">>,
                insertText := InsertText
            } <- Completion2
        ]
    ),
    ct:comment("Completing function at code_navigation:|(), shouldn't include args"),
    #{result := Completion3} =
        els_client:completion(Uri, 27, 19, ?COMPLETION_TRIGGER_KIND_CHARACTER, <<":">>),
    ?assertEqual(
        [<<"function_a">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_FUNCTION,
                label := <<"function_a/0">>,
                insertText := InsertText
            } <- Completion3
        ]
    ),
    ct:comment("Completing function at code_navigation:funct|(), shouldn't include args"),
    #{result := Completion4} =
        els_client:completion(Uri, 28, 24, TriggerKind, <<>>),
    ?assertEqual(
        [<<"function_a">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_FUNCTION,
                insertText := InsertText,
                label := <<"function_a/0">>
            } <- Completion4
        ]
    ).

-spec handle_empty_lines(config()) -> ok.
handle_empty_lines(Config) ->
    Uri = ?config(code_navigation_uri, Config),
    TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

    #{result := Completion1} = els_client:completion(Uri, 32, 1, TriggerKind, <<"">>),
    ?assertEqual([], Completion1),

    #{result := Completion2} = els_client:completion(Uri, 32, 2, TriggerKind, <<":">>),
    ?assertEqual([], Completion2),

    ok.

-spec handle_colon_inside_string(config()) -> ok.
handle_colon_inside_string(Config) ->
    Uri = ?config(code_navigation_uri, Config),
    TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,

    #{result := Completion} = els_client:completion(Uri, 76, 10, TriggerKind, <<":">>),
    ?assertEqual([], Completion),

    ok.

-spec macros(config()) -> ok.
macros(Config) ->
    Uri = ?config(code_navigation_uri, Config),
    TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected = [
        #{
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"INCLUDED_MACRO_A">>,
            insertText => <<"INCLUDED_MACRO_A">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"MACRO_A">>,
            insertText => <<"MACRO_A">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"MACRO_A/1">>,
            insertText => <<"MACRO_A(${1:X})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"macro_A">>,
            insertText => <<"macro_A">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_CONSTANT,
            label => <<"MACRO_FOR_TRANSITIVE_INCLUSION">>,
            insertText => <<"MACRO_FOR_TRANSITIVE_INCLUSION">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{}
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
    Expected = [
        #{
            kind => ?COMPLETION_ITEM_KIND_STRUCT,
            label => <<"'?MODULE'">>,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_STRUCT,
            label => <<"included_record_a">>,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_STRUCT,
            label => <<"record_a">>,
            data => #{}
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_STRUCT,
            label => <<"'PascalCaseRecord'">>,
            data => #{}
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
    Expected1 = [
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_a">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_b">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"'Field C'">>
        }
    ],
    #{result := Completion1} =
        els_client:completion(Uri, 34, 19, TriggerKindChar, <<".">>),
    ?assertEqual(lists:sort(Expected1), lists:sort(Completion1)),

    #{result := Completion2} =
        els_client:completion(Uri, 34, 22, TriggerKindInvoked, <<"">>),
    ?assertEqual(lists:sort(Expected1), lists:sort(Completion2)),

    Expected2 = [
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"included_field_a">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"included_field_b">>
        }
    ],
    #{result := Completion3} =
        els_client:completion(Uri, 52, 60, TriggerKindChar, <<".">>),
    ?assertEqual(lists:sort(Expected2), lists:sort(Completion3)),

    #{result := Completion4} =
        els_client:completion(Uri, 52, 63, TriggerKindInvoked, <<"">>),
    ?assertEqual(lists:sort(Expected2), lists:sort(Completion4)),

    ExpectedQuoted = [
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"'Field #1'">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"'Field #2'">>
        }
    ],
    #{result := Completion5} =
        els_client:completion(Uri, 52, 90, TriggerKindChar, <<".">>),
    ?assertEqual(lists:sort(ExpectedQuoted), lists:sort(Completion5)),

    ok.

-spec record_fields_inside_record(config()) -> ok.
record_fields_inside_record(Config) ->
    Uri = ?config(completion_records_uri, Config),
    TriggerKindChar = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    TriggerKindInvoked = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Expected1 = [
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_a">>,
            insertText => <<"field_a = ${1:FieldA}">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_b">>,
            insertText => <<"field_b = ${1:FieldB}">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"'Field C'">>,
            insertText => <<"'Field C' = ${1:Field C}">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
        }
    ],
    Expected2 = [
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_x">>,
            insertText => <<"field_x = ${1:FieldX}">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_FIELD,
            label => <<"field_y">>,
            insertText => <<"field_y = ${1:FieldY}">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
        }
    ],
    %% Completion of #record_a in function head
    %% #record_a{|
    #{result := Completion1} =
        els_client:completion(Uri, 6, 22, TriggerKindChar, <<"{">>),

    %% Completion of #record_a in function head
    %% #record_a{field_a = a, |
    #{result := Completion1} =
        els_client:completion(Uri, 6, 35, TriggerKindChar, <<" ">>),

    %% Completion of #record_a in function head
    %% #record_a{|
    #{result := Completion1} =
        els_client:completion(Uri, 6, 22, TriggerKindInvoked, <<>>),

    %% Completion of #record_a in function head
    %% #record_a{fiel|
    #{result := Completion1} =
        els_client:completion(Uri, 6, 26, TriggerKindInvoked, <<>>),

    %% Completion of #record_b in function body
    %% #record_b{fi|
    #{result := Completion2} =
        els_client:completion(Uri, 7, 16, TriggerKindInvoked, <<>>),

    %% Completion of #record_a inside #record_b
    %% #record_b{field_x = #record_a{|
    #{result := Completion1} =
        els_client:completion(Uri, 7, 35, TriggerKindInvoked, <<>>),

    %% Completion of #record_a in function head
    %% #record_b{field_x = #record_a{|
    #{result := Completion1} =
        els_client:completion(Uri, 7, 35, TriggerKindChar, <<"{">>),

    %% Completion of #record_b
    %% #record_b{field_x = #record_a{}, |
    #{result := Completion2} =
        els_client:completion(Uri, 7, 38, TriggerKindChar, <<" ">>),

    %% Records in comments are ignored
    #{result := Completion2} =
        els_client:completion(Uri, 9, 16, TriggerKindInvoked, <<>>),

    %% No completion when trying to complete inside a tuple
    #{result := []} =
        els_client:completion(Uri, 10, 6, TriggerKindChar, <<"{">>),
    #{result := []} =
        els_client:completion(Uri, 10, 6, TriggerKindInvoked, <<>>),

    %% #record_b{field_y = y|} invoke completion
    %% shouldn't trigger record fields completion
    #{result := Completion3} =
        els_client:completion(Uri, 9, 26, TriggerKindInvoked, <<>>),
    ?assertNotEqual(lists:sort(Expected1), Completion3),
    ?assertNotEqual(lists:sort(Expected2), Completion3),
    ?assertEqual(lists:sort(Expected1), lists:sort(Completion1)),
    ?assertEqual(lists:sort(Expected2), lists:sort(Completion2)),
    ok.

-spec types(config()) -> ok.
types(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_uri, Config),
    Expected = [
        #{
            insertText => <<"type_a()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"type_a/0">>,
            data => #{
                module => <<"code_navigation">>,
                type => <<"type_a">>,
                arity => 0
            }
        },
        #{
            insertText => <<"included_type_a()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"included_type_a/0">>,
            data => #{
                module => <<"code_navigation">>,
                type => <<"included_type_a">>,
                arity => 0
            }
        },
        #{
            insertText => <<"'INCLUDED_TYPE'(${1:T})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"'INCLUDED_TYPE'/1">>,
            data => #{
                module => <<"code_navigation">>,
                type => <<"INCLUDED_TYPE">>,
                arity => 1
            }
        },
        #{
            insertText => <<"type_b()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"type_b/0">>,
            data => #{
                module => <<"code_navigation">>,
                type => <<"type_b">>,
                arity => 0
            }
        }
    ],

    DefaultCompletion =
        els_completion_provider:bifs(type_definition, args) ++
            els_snippets_server:snippets(),

    ct:comment("Types defined both in the current file and in includes"),
    #{result := Completion1} =
        els_client:completion(Uri, 55, 27, TriggerKind, <<"">>),
    ?assertEqual(
        [],
        Expected -- (Completion1 -- DefaultCompletion)
    ),
    ?assertEqual(
        [],
        (Completion1 -- DefaultCompletion) -- Expected
    ),
    ok.

-spec types_export_list(config()) -> ok.
types_export_list(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_types_uri, Config),
    Expected = [
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"user_type_a/0">>,
            data => #{
                module => <<"code_navigation_types">>,
                type => <<"user_type_a">>,
                arity => 0
            }
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"user_type_b/0">>,
            data => #{
                module => <<"code_navigation_types">>,
                type => <<"user_type_b">>,
                arity => 0
            }
        },
        #{
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
            label => <<"user_type_d/0">>,
            data => #{
                module => <<"code_navigation_types">>,
                type => <<"user_type_d">>,
                arity => 0
            }
        }
    ],
    ct:comment("Types in an export_type section is provided with arity"),
    #{result := Completion} =
        els_client:completion(Uri, 5, 19, TriggerKind, <<"">>),
    ?assertEqual(Expected, Completion),
    ok.

-spec types_context(config()) -> ok.
types_context(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_types_uri, Config),
    UserTypes =
        [
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"opaque_type_a">>
                    },
                insertText => <<"opaque_type_a()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"opaque_type_a/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"type_a">>
                    },
                insertText => <<"type_a()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"type_a/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"type_b">>
                    },
                insertText => <<"type_b()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"type_b/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"user_type_a">>
                    },
                insertText => <<"user_type_a()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"user_type_a/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"user_type_b">>
                    },
                insertText => <<"user_type_b()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"user_type_b/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"user_type_c">>
                    },
                insertText => <<"user_type_c()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"user_type_c/0">>
            },
            #{
                data =>
                    #{
                        arity => 0,
                        module => <<"code_navigation_types">>,
                        type => <<"user_type_d">>
                    },
                insertText => <<"user_type_d()">>,
                insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
                kind => ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label => <<"user_type_d/0">>
            }
        ],
    Bifs = els_completion_provider:bifs(type_definition, args),
    Expected1 = UserTypes ++ Bifs,

    ct:comment("Completing a type inside a -type declaration should return types"),
    #{result := Completion1} =
        els_client:completion(Uri, 16, 27, TriggerKind, <<>>),
    ?assertEqual([], Completion1 -- Expected1),
    ?assertEqual([], Expected1 -- Completion1),

    ct:comment("Completing a map value inside a -type should return types"),
    #{result := Completion2} =
        els_client:completion(Uri, 21, 43, TriggerKind, <<>>),
    ?assertEqual([], Completion2 -- Expected1),
    ?assertEqual([], Expected1 -- Completion2),

    ct:comment("Completing a type in a record definition should return types"),
    #{result := Completion1} =
        els_client:completion(Uri, 18, 38, TriggerKind, <<>>),

    ct:comment("Completing a record value should not return types"),
    #{result := Completion4} =
        els_client:completion(Uri, 18, 23, TriggerKind, <<>>),
    ?assertNotEqual([], Completion4),
    ?assertEqual(UserTypes, UserTypes -- Completion4),

    ct:comment("Completing a type in a spec return types"),
    #{result := Completion5} =
        els_client:completion(Uri, 24, 31, TriggerKind, <<>>),
    #{result := Completion5} =
        els_client:completion(Uri, 24, 54, TriggerKind, <<>>),
    ?assertEqual([], Completion5 -- Expected1),
    ?assertEqual([], Expected1 -- Completion5),

    ct:comment("Completing a value in function head should not return types"),
    #{result := Completion6} =
        els_client:completion(Uri, 25, 15, TriggerKind, <<>>),
    ?assertNotEqual([], Completion6),
    ?assertEqual(UserTypes, UserTypes -- Completion6),

    ct:comment("Completing a value in function body should not return types"),
    #{result := Completion6} =
        els_client:completion(Uri, 26, 8, TriggerKind, <<>>),
    ?assertNotEqual([], Completion6),
    ?assertEqual(UserTypes, UserTypes -- Completion6),
    ok.

-spec types_no_args(config()) -> ok.
types_no_args(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_types_uri, Config),
    ct:comment("Completing function at typ|e(), should include args"),
    #{result := Completion1} =
        els_client:completion(Uri, 28, 27, TriggerKind, <<>>),
    ?assertEqual(
        [<<"type_a()">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label := <<"type_a/0">>,
                insertText := InsertText
            } <- Completion1
        ]
    ),
    ct:comment("Completing function at type|(), shouldn't include args"),
    #{result := Completion2} =
        els_client:completion(Uri, 28, 28, TriggerKind, <<>>),
    ?assertEqual(
        [<<"type_a">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label := <<"type_a/0">>,
                insertText := InsertText
            } <- Completion2
        ]
    ),
    ct:comment("Completing function at code_navigation:|(), shouldn't include args"),
    #{result := Completion3} =
        els_client:completion(Uri, 28, 49, ?COMPLETION_TRIGGER_KIND_CHARACTER, <<":">>),
    ?assertEqual(
        [<<"type_a">>],
        [
            InsertText
         || #{
                kind := ?COMPLETION_ITEM_KIND_TYPE_PARAM,
                label := <<"type_a/0">>,
                insertText := InsertText
            } <- Completion3
        ]
    ).

-spec variables(config()) -> ok.
variables(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    Uri = ?config(code_navigation_extra_uri, Config),
    Expected = [
        #{
            kind => ?COMPLETION_ITEM_KIND_VARIABLE,
            label => <<"_Config">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_VARIABLE,
            label => <<"Arg1">>
        },
        #{
            kind => ?COMPLETION_ITEM_KIND_VARIABLE,
            label => <<"Arg2">>
        }
    ],

    #{result := Completion} =
        els_client:completion(Uri, 5, 8, TriggerKind, <<"">>),
    ?assertEqual(lists:sort(Expected), lists:sort(Completion)),

    ok.

expected_exported_functions() ->
    [
        #{
            label => <<"do/1">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertText => <<"do(${1:_Config})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do">>,
                arity => 1
            }
        },
        #{
            label => <<"do_2/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertText => <<"do_2()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do_2">>,
                arity => 0
            }
        },
        #{
            label => <<"'DO_LOUDER'/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertText => <<"'DO_LOUDER'()">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"DO_LOUDER">>,
                arity => 0
            }
        },
        #{
            label => <<"function_a/2">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertText => <<"function_a(${1:Arg1}, ${2:Arg2})">>,
            insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"function_a">>,
                arity => 2
            }
        }
    ].

expected_exported_functions_arity_only() ->
    [
        #{
            label => <<"do/1">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do">>,
                arity => 1
            }
        },
        #{
            label => <<"do_2/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"do_2">>,
                arity => 0
            }
        },
        #{
            label => <<"'DO_LOUDER'/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"DO_LOUDER">>,
                arity => 0
            }
        },
        #{
            label => <<"function_a/2">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"code_navigation_extra">>,
                function => <<"function_a">>,
                arity => 2
            }
        }
    ].

%% [#790] Complete only with arity for remote applications
-spec remote_fun(config()) -> ok.
remote_fun(Config) ->
    TriggerKind = ?COMPLETION_TRIGGER_KIND_CHARACTER,
    Uri = ?config(completion_caller_uri, Config),
    ExpectedCompletion = [
        #{
            label => <<"complete_1/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"completion">>,
                function => <<"complete_1">>,
                arity => 0
            }
        },
        #{
            label => <<"complete_2/0">>,
            kind => ?COMPLETION_ITEM_KIND_FUNCTION,
            insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT,
            data => #{
                module => <<"completion">>,
                function => <<"complete_2">>,
                arity => 0
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
        els_client:completion(Uri, 14, 5, CompletionKind, <<"">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"call_1/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => call_markdown(
                    <<"call_1">>,
                    <<"Call me maybe">>
                )
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_application_unexported_local(config()) -> ok.
resolve_application_unexported_local(Config) ->
    Uri = ?config(completion_resolve_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 14, 5, CompletionKind, <<"">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"call_2/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => call_markdown(
                    <<"call_2">>,
                    <<"Call me sometime">>
                )
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_application_remote_self(config()) -> ok.
resolve_application_remote_self(Config) ->
    Uri = ?config(completion_resolve_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 13, 23, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"call_1/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),

    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => call_markdown(
                    <<"call_1">>,
                    <<"Call me maybe">>
                )
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_application_remote_external(config()) -> ok.
resolve_application_remote_external(Config) ->
    Uri = ?config(completion_resolve_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 15, 25, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"call_1/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => call_markdown(
                    <<"completion_resolve_2">>,
                    <<"call_1">>,
                    <<"I just met you">>
                )
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_application_remote_otp(config()) -> ok.
resolve_application_remote_otp(Config) ->
    Uri = ?config(completion_resolve_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 16, 8, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_FUNCTION,
        <<"write/2">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    OtpRelease = list_to_integer(erlang:system_info(otp_release)),
    Value =
        case has_eep48(file) of
            true when OtpRelease >= 26 ->
                <<
                    "```erlang\nwrite(IoDevice, Bytes) -> ok | {error, "
                    "Reason}\nwhen\n  IoDevice :: io_device() | io:device(),\n"
                    "  Bytes :: iodata(),"
                    "\n  Reason :: posix() | badarg | terminated.\n```\n\n"
                    "---\n\n"
                    "Writes `Bytes` to the file referenced by `IoDevice`\\."
                    " This function is the only way to write to a file opened in"
                    " `raw` mode \\(although it works for normally opened files"
                    " too\\)\\. Returns `ok` if successful, and"
                    " `{error, Reason}` otherwise\\.\n\nIf the file is opened"
                    " with `encoding` set to something else than `latin1`,"
                    " each byte written can result in many bytes being written"
                    " to the file, as the byte range 0\\.\\.255 can represent"
                    " anything between one and four bytes depending on value"
                    " and UTF encoding type\\. If you want to write"
                    " [`unicode:chardata()`](https://erlang.org/doc/man/unicode"
                    ".html#type-chardata) to the `IoDevice` you should use"
                    " [`io:put_chars/2`](https://erlang.org/doc/man/io.html"
                    "#put_chars-2) instead\\.\n\nTypical error reasons:\n\n"
                    "* **`ebadf`**  \n  The file is not opened for writing\\.\n\n"
                    "* **`enospc`**  \n  No space is left on the device\\.\n"
                >>;
            true ->
                <<
                    "```erlang\nwrite(IoDevice, Bytes) -> ok | {error, "
                    "Reason}\nwhen\n  IoDevice :: io_device() | atom(),\n  Bytes ::"
                    " iodata(),\n  Reason :: posix() | badarg | terminated.\n```\n\n"
                    "---\n\nWrites `Bytes` to the file referenced by `IoDevice`\\. "
                    "This function is the only way to write to a file opened in `raw`"
                    " mode \\(although it works for normally opened files too\\)\\. "
                    "Returns `ok` if successful, and `{error, Reason}` otherwise\\."
                    "\n\nIf the file is opened with `encoding` set to something else "
                    "than `latin1`, each byte written can result in many bytes being "
                    "written to the file, as the byte range 0\\.\\.255 can represent "
                    "anything between one and four bytes depending on value and UTF "
                    "encoding type\\.\n\nTypical error reasons:\n\n* **`ebadf`**  \n"
                    "  The file is not opened for writing\\.\n\n* **`enospc`**  \n"
                    "  No space is left on the device\\.\n"
                >>;
            false ->
                <<
                    "## file:write/2\n\n---\n\n```erlang\n\n  write(File, "
                    "Bytes) when is_pid(File) orelse is_atom(File)\n\n  write(#file_"
                    "descriptor{module = Module} = Handle, Bytes) \n\n  write(_, _) "
                    "\n\n```\n\n```erlang\n-spec write(IoDevice, Bytes) -> ok | "
                    "{error, Reason} when\n      IoDevice :: io_device() | atom(),"
                    "\n      Bytes :: iodata(),\n      Reason :: posix() | "
                    "badarg | terminated.\n```"
                >>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

call_markdown(F, Doc) ->
    call_markdown(<<"completion_resolve">>, F, Doc).
call_markdown(M, F, Doc) ->
    case has_eep48_edoc() of
        true ->
            <<"```erlang\n", F/binary,
                "() -> ok.\n"
                "```\n\n"
                "---\n\n", Doc/binary, "\n">>;
        false ->
            <<"## ", M/binary, ":", F/binary,
                "/0\n\n"
                "---\n\n"
                "```erlang\n"
                "-spec ", F/binary,
                "() -> 'ok'.\n"
                "```\n\n", Doc/binary, "\n\n">>
    end.

-spec resolve_type_application_local(config()) -> ok.
resolve_type_application_local(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 14, 16, CompletionKind, <<"">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"mytype/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Value =
        case has_eep48_edoc() of
            true ->
                <<
                    "```erlang\n-type mytype() :: "
                    "completion_resolve_type:myopaque().\n```"
                    "\n\n---\n\nThis is my type\n"
                >>;
            false ->
                <<
                    "```erlang\n-type mytype() :: "
                    "completion_resolve_type:myopaque().\n```"
                >>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_opaque_application_local(config()) -> ok.
resolve_opaque_application_local(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 14, 17, CompletionKind, <<"">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"myopaque/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Value =
        case has_eep48_edoc() of
            true ->
                <<
                    "```erlang\n-opaque myopaque() \n```\n\n---\n\n"
                    "This is my opaque\n"
                >>;
            false ->
                <<
                    "```erlang\n"
                    "-opaque myopaque() :: term().\n```"
                >>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_opaque_application_remote_self(config()) -> ok.
resolve_opaque_application_remote_self(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 14, 48, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"myopaque/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),

    Value =
        case has_eep48_edoc() of
            true ->
                <<
                    "```erlang\n-opaque myopaque() \n```\n\n---\n\n"
                    "This is my opaque\n"
                >>;
            false ->
                <<
                    "```erlang\n"
                    "-opaque myopaque() :: term().\n"
                    "```"
                >>
        end,

    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_type_application_remote_external(config()) -> ok.
resolve_type_application_remote_external(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 15, 40, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"mytype/1">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Value =
        case has_eep48_edoc() of
            true ->
                <<
                    "```erlang\n-type mytype(T) :: [T].\n```\n\n---\n\n"
                    "Hello\n"
                >>;
            false ->
                <<"```erlang\n-type mytype(T) :: [T].\n```">>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_opaque_application_remote_external(config()) -> ok.
resolve_opaque_application_remote_external(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 15, 40, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"myopaque/1">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Value =
        case has_eep48_edoc() of
            true ->
                <<
                    "```erlang\n-opaque myopaque(T) \n```\n\n---\n\n"
                    "Is there anybody in there\n"
                >>;
            false ->
                <<"```erlang\n-opaque myopaque(T) :: [T].\n```">>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

-spec resolve_type_application_remote_otp(config()) -> ok.
resolve_type_application_remote_otp(Config) ->
    Uri = ?config(completion_resolve_type_uri, Config),
    CompletionKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    #{result := CompletionItems} =
        els_client:completion(Uri, 17, 8, CompletionKind, <<":">>),
    [Selected] = select_completionitems(
        CompletionItems,
        ?COMPLETION_ITEM_KIND_TYPE_PARAM,
        <<"name_all/0">>
    ),
    #{result := Result} = els_client:completionitem_resolve(Selected),
    Value =
        case has_eep48(file) of
            true ->
                <<
                    "```erlang\n-type name_all() ::\n     string() |"
                    " atom() | deep_list() | (RawFilename :: binary()).\n"
                    "```\n\n---\n\nIf VM is in Unicode filename mode, "
                    "characters are allowed to be \\> 255\\. `RawFilename`"
                    " is a filename not subject to Unicode translation, "
                    "meaning that it can contain characters not conforming"
                    " to the Unicode encoding expected from the file system"
                    " \\(that is, non\\-UTF\\-8 characters although the VM is"
                    " started in Unicode filename mode\\)\\. Null characters "
                    "\\(integer value zero\\) are *not* allowed in filenames "
                    "\\(not even at the end\\)\\.\n"
                >>;
            false ->
                <<
                    "```erlang\n-type name_all()  :: "
                    "string() | atom() | deep_list() | "
                    "(RawFilename :: binary()).\n```"
                >>
        end,
    Expected = Selected#{
        documentation =>
            #{
                kind => <<"markdown">>,
                value => Value
            }
    },
    ?assertEqual(Expected, Result).

%% Issue #1387
completion_request_fails(Config) ->
    Uri = ?config(code_completion_fail_uri, Config),
    TriggerKind = ?COMPLETION_TRIGGER_KIND_INVOKED,
    %% Complete at -module(s|).
    #{result := Result1} = els_client:completion(Uri, 1, 10, TriggerKind, <<>>),
    ?assertNotEqual(null, Result1),
    %% Complete at a|, file doesn't end with a newline!
    #{result := Result2} = els_client:completion(Uri, 2, 2, TriggerKind, <<>>),
    ?assertNotEqual(null, Result2),
    ok.

select_completionitems(CompletionItems, Kind, Label) ->
    [CI || #{kind := K, label := L} = CI <- CompletionItems, L =:= Label, K =:= Kind].

has_eep48_edoc() ->
    list_to_integer(erlang:system_info(otp_release)) >= 24.

has_eep48(Module) ->
    case catch code:get_doc(Module) of
        {ok, {docs_v1, _, erlang, _, _, _, Docs}} ->
            lists:any(
                fun
                    ({_, _, _, Doc, _}) when is_map(Doc) -> true;
                    ({_, _, _, _, _}) -> false
                end,
                Docs
            );
        _ ->
            false
    end.

keywords() ->
    els_completion_provider:keywords(test, test).

-if(OTP_RELEASE >= 26).
file_exported_types() ->
    [
        <<"date_time">>,
        <<"fd">>,
        <<"file_info">>,
        <<"filename">>,
        <<"filename_all">>,
        <<"io_device">>,
        <<"location">>,
        <<"mode">>,
        <<"name">>,
        <<"name_all">>,
        <<"posix">>
    ].
-else.
file_exported_types() ->
    [
        <<"date_time">>,
        <<"fd">>,
        <<"file_info">>,
        <<"filename">>,
        <<"filename_all">>,
        <<"io_device">>,
        <<"mode">>,
        <<"name">>,
        <<"name_all">>,
        <<"posix">>
    ].
-endif.
