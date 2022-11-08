-module(els_code_action_SUITE).

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
    add_underscore_to_unused_var/1,
    export_unused_function/1,
    suggest_variable/1,
    fix_module_name/1,
    remove_unused_macro/1,
    remove_unused_import/1,
    create_undefined_function/1,
    create_undefined_function_arity/1,
    fix_callbacks/1
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
%% Const
%%==============================================================================
-define(COMMENTS_LINES, 2).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec add_underscore_to_unused_var(config()) -> ok.
add_underscore_to_unused_var(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 6, 3},
        to => {?COMMENTS_LINES + 6, 4}
    }),
    Diag = #{
        message => <<"variable 'A' is unused">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range => Range,
                                        newText => <<"_A">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Add '_' to 'A'">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec export_unused_function(config()) -> ok.
export_unused_function(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 12, 1},
        to => {?COMMENTS_LINES + 12, 10}
    }),
    Diag = #{
        message => <<"function function_c/0 is unused">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range =>
                                            #{
                                                'end' => #{
                                                    character => 0,
                                                    line => ?COMMENTS_LINES + 3
                                                },
                                                start => #{
                                                    character => 0,
                                                    line => ?COMMENTS_LINES + 3
                                                }
                                            },
                                        newText => <<"-export([function_c/0]).\n">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Export function_c/0">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec suggest_variable(config()) -> ok.
suggest_variable(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 15, 9},
        to => {?COMMENTS_LINES + 15, 13}
    }),
    Diag = #{
        message => <<"variable 'Barf' is unbound">>,
        range => Range,
        severity => 3,
        source => <<"Compiler">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range => Range,
                                        newText => <<"Bar">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Did you mean 'Bar'?">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec fix_module_name(config()) -> ok.
fix_module_name(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 1, 9},
        to => {?COMMENTS_LINES + 1, 25}
    }),
    Diag = #{
        message => <<
            "Module name 'code_action_oops' does not "
            "match file name 'code_action'"
        >>,
        range => Range,
        severity => 3,
        source => <<"Compiler">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range => Range,
                                        newText => <<"code_action">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Change to -module(code_action).">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec remove_unused_macro(config()) -> ok.
remove_unused_macro(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    LineRange = els_range:line(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    Diag = #{
        message => <<"Unused macro: TIMEOUT">>,
        range => Range,
        severity => 2,
        source => <<"UnusedMacros">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range => els_protocol:range(LineRange),
                                        newText => <<"">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Remove unused macro TIMEOUT.">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec remove_unused_import(config()) -> ok.
remove_unused_import(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 19, 15},
        to => {?COMMENTS_LINES + 19, 40}
    }),
    LineRange = els_range:line(#{
        from => {?COMMENTS_LINES + 19, 15},
        to => {?COMMENTS_LINES + 19, 40}
    }),
    {ok, FileName} = els_utils:find_header(
        els_utils:filename_to_atom("stdlib/include/assert.hrl")
    ),
    Diag = #{
        message => <<"Unused file: assert.hrl">>,
        range => Range,
        severity => 2,
        source => <<"UnusedIncludes">>,
        data => FileName
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range => els_protocol:range(LineRange),
                                        newText => <<>>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Remove unused -include_lib(assert.hrl).">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec create_undefined_function((config())) -> ok.
create_undefined_function(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {23, 2},
        to => {23, 8}
    }),
    Diag = #{
        message => <<"function foobar/0 undefined">>,
        range => Range,
        severity => 2,
        source => <<"">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range =>
                                            els_protocol:range(#{
                                                from => {27, 1},
                                                to => {27, 1}
                                            }),
                                        newText =>
                                            <<"foobar() ->\n  ok.\n\n">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Create function foobar/0">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec create_undefined_function_arity((config())) -> ok.
create_undefined_function_arity(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {24, 2},
        to => {24, 8}
    }),
    Diag = #{
        message => <<"function foobar/3 undefined">>,
        range => Range,
        severity => 2,
        source => <<"">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        [
            #{
                edit => #{
                    changes =>
                        #{
                            binary_to_atom(Uri, utf8) =>
                                [
                                    #{
                                        range =>
                                            els_protocol:range(#{
                                                from => {27, 1},
                                                to => {27, 1}
                                            }),
                                        newText =>
                                            <<"foobar(_, _, _) ->\n  ok.\n\n">>
                                    }
                                ]
                        }
                },
                kind => <<"quickfix">>,
                title => <<"Create function foobar/3">>
            }
        ],
    ?assertEqual(Expected, Result),
    ok.

-spec fix_callbacks(config()) -> ok.
fix_callbacks(Config) ->
    Uri = ?config(code_action_uri, Config),
    % Ignored
    Range = els_protocol:range(#{from => {4, 1}, to => {4, 15}}),
    Diag = #{
        message => <<"undefined callback function init/1 \(behaviour 'gen_server'\)">>,
        range => Range,
        severity => 3,
        source => <<"Compiler">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Title = <<"Add missing callbacks for: gen_server">>,
    Command = els_command:with_prefix(<<"add-behaviour-callbacks">>),
    ?assertMatch(
        [
            #{
                command := #{
                    title := Title,
                    command := Command,
                    arguments :=
                        [
                            #{
                                uri := Uri,
                                behaviour := <<"gen_server">>
                            }
                        ]
                },
                kind := <<"quickfix">>,
                title := Title
            }
        ],
        Result
    ),
    ok.
