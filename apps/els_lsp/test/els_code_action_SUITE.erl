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
    create_undefined_function_variable_names/1,
    fix_callbacks/1,
    extract_function/1,
    add_include_file_macro/1,
    define_macro/1,
    define_macro_with_args/1,
    suggest_macro/1,
    undefined_record/1,
    undefined_record_suggest/1
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
        },
    ?assert(lists:member(Expected, Result)),
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
        from => {23, 3},
        to => {23, 9}
    }),
    Diag = #{
        message => <<"function foobar/0 undefined">>,
        range => Range,
        severity => 2,
        source => <<"">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        #{
            edit => #{
                changes =>
                    #{
                        binary_to_atom(Uri, utf8) =>
                            [
                                #{
                                    range =>
                                        els_protocol:range(#{
                                            from => {28, 1},
                                            to => {28, 1}
                                        }),
                                    newText =>
                                        <<"foobar() ->\n  ok.\n\n">>
                                }
                            ]
                    }
            },
            kind => <<"quickfix">>,
            title => <<"Create function foobar/0">>
        },
    ?assert(lists:member(Expected, Result)),
    ok.

-spec create_undefined_function_arity((config())) -> ok.
create_undefined_function_arity(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {24, 3},
        to => {24, 9}
    }),
    Diag = #{
        message => <<"function foobar/3 undefined">>,
        range => Range,
        severity => 2,
        source => <<"">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        #{
            edit => #{
                changes =>
                    #{
                        binary_to_atom(Uri, utf8) =>
                            [
                                #{
                                    range =>
                                        els_protocol:range(#{
                                            from => {28, 1},
                                            to => {28, 1}
                                        }),
                                    newText =>
                                        <<"foobar(Arg1, Arg2, Arg3) ->\n  ok.\n\n">>
                                }
                            ]
                    }
            },
            kind => <<"quickfix">>,
            title => <<"Create function foobar/3">>
        },
    ?assert(lists:member(Expected, Result)),
    ok.

-spec create_undefined_function_variable_names((config())) -> ok.
create_undefined_function_variable_names(Config) ->
    Uri = ?config(code_action_uri, Config),
    Range = els_protocol:range(#{
        from => {25, 3},
        to => {25, 9}
    }),
    Diag = #{
        message => <<"function foobar/5 undefined">>,
        range => Range,
        severity => 2,
        source => <<"">>
    },
    #{result := Result} = els_client:document_codeaction(Uri, Range, [Diag]),
    Expected =
        #{
            edit => #{
                changes =>
                    #{
                        binary_to_atom(Uri, utf8) =>
                            [
                                #{
                                    range =>
                                        els_protocol:range(#{
                                            from => {28, 1},
                                            to => {28, 1}
                                        }),
                                    newText =>
                                        <<"foobar(Foo, FooBar, Bar, FooBar) ->\n  ok.\n\n">>
                                }
                            ]
                    }
            },
            kind => <<"quickfix">>,
            title => <<"Create function foobar/5">>
        },
    ?assert(lists:member(Expected, Result)),
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

-spec extract_function(config()) -> ok.
extract_function(Config) ->
    Uri = ?config(extract_function_uri, Config),
    %% These shouldn't return any code actions
    #{result := []} = els_client:document_codeaction(
        Uri,
        els_protocol:range(#{from => {2, 1}, to => {2, 5}}),
        []
    ),
    #{result := []} = els_client:document_codeaction(
        Uri,
        els_protocol:range(#{from => {3, 1}, to => {3, 5}}),
        []
    ),
    #{result := []} = els_client:document_codeaction(
        Uri,
        els_protocol:range(#{from => {4, 1}, to => {4, 5}}),
        []
    ),
    #{result := []} = els_client:document_codeaction(
        Uri,
        els_protocol:range(#{from => {5, 8}, to => {5, 9}}),
        []
    ),
    %% This should return a code action
    #{
        result := [
            #{
                command := #{
                    title := <<"Extract function">>,
                    arguments := [#{uri := Uri}]
                },
                kind := <<"refactor.extract">>,
                title := <<"Extract function">>
            }
        ]
    } = els_client:document_codeaction(
        Uri,
        els_protocol:range(#{from => {5, 8}, to => {5, 17}}),
        []
    ),
    ok.

-spec add_include_file_macro(config()) -> ok.
add_include_file_macro(Config) ->
    Uri = ?config(code_action_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    DestRange = els_protocol:range(#{
        from => {4, 1},
        to => {4, 1}
    }),
    Diag = #{
        message => <<"undefined macro 'INCLUDED_MACRO_A'">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Path = <<"code_navigation/include/code_navigation.hrl">>,
    Changes =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-include_lib(\"", Path/binary, "\").\n">>
                    }
                ]
        },
    Expected =
        #{
            edit => #{changes => Changes},
            kind => <<"quickfix">>,
            title => <<"Add -include_lib(\"", Path/binary, "\")">>
        },
    ?assert(lists:member(Expected, Result)),
    ok.

-spec define_macro(config()) -> ok.
define_macro(Config) ->
    Uri = ?config(code_action_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    DestRange = els_protocol:range(#{
        from => {4, 1},
        to => {4, 1}
    }),
    Diag = #{
        message => <<"undefined macro 'MAGIC_MACRO'">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Changes =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-define(MAGIC_MACRO, undefined).\n">>
                    }
                ]
        },
    Expected = [
        #{
            edit => #{changes => Changes},
            kind => <<"quickfix">>,
            title => <<"Define MAGIC_MACRO">>
        }
    ],
    ?assertEqual(Expected, Result),
    ok.

-spec suggest_macro(config()) -> ok.
suggest_macro(Config) ->
    Uri = ?config(code_action_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    Diag = #{
        message => <<"undefined macro 'assertEql/2'">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Changes =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => Range,
                        newText => <<"?assertEqual">>
                    }
                ]
        },
    Expected =
        #{
            edit => #{changes => Changes},
            kind => <<"quickfix">>,
            title => <<"Did you mean 'assertEqual'?">>
        },
    ?assert(lists:member(Expected, Result)),
    ok.

-spec define_macro_with_args(config()) -> ok.
define_macro_with_args(Config) ->
    Uri = ?config(code_action_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    DestRange = els_protocol:range(#{
        from => {4, 1},
        to => {4, 1}
    }),
    Diag = #{
        message => <<"undefined macro 'MAGIC_MACRO/2'">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Changes =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-define(MAGIC_MACRO(_, _), undefined).\n">>
                    }
                ]
        },
    Expected = [
        #{
            edit => #{changes => Changes},
            kind => <<"quickfix">>,
            title => <<"Define MAGIC_MACRO/2">>
        }
    ],
    ?assertEqual(Expected, Result),
    ok.

-spec undefined_record(config()) -> ok.
undefined_record(Config) ->
    Uri = ?config(code_action_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {?COMMENTS_LINES + 17, 9},
        to => {?COMMENTS_LINES + 17, 15}
    }),
    DestRange = els_protocol:range(#{
        from => {4, 1},
        to => {4, 1}
    }),
    Diag = #{
        message => <<"record included_record_a undefined">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Path1 = <<"code_navigation/include/code_navigation.hrl">>,
    Path2 = <<"code_navigation/include/hover_record.hrl">>,
    Changes1 =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-include_lib(\"", Path1/binary, "\").\n">>
                    }
                ]
        },
    Changes2 =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-include_lib(\"", Path2/binary, "\").\n">>
                    }
                ]
        },
    Changes3 =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-record(included_record_a, {}).\n">>
                    }
                ]
        },
    Expected = [
        #{
            edit => #{changes => Changes1},
            kind => <<"quickfix">>,
            title => <<"Add -include_lib(\"", Path1/binary, "\")">>
        },
        #{
            edit => #{changes => Changes2},
            kind => <<"quickfix">>,
            title => <<"Add -include_lib(\"", Path2/binary, "\")">>
        },
        #{
            edit => #{changes => Changes3},
            kind => <<"quickfix">>,
            title => <<"Define record included_record_a">>
        }
    ],
    ?assertEqual(Expected, Result),
    ok.

-spec undefined_record_suggest(config()) -> ok.
undefined_record_suggest(Config) ->
    Uri = ?config(undefined_record_suggest_uri, Config),
    %% Don't care
    Range = els_protocol:range(#{
        from => {5, 4},
        to => {5, 11}
    }),
    DestRange = els_protocol:range(#{
        from => {4, 1},
        to => {4, 1}
    }),
    Diag = #{
        message => <<"record foo_bar undefined">>,
        range => Range,
        severity => 2,
        source => <<"Compiler">>
    },
    #{result := Result} =
        els_client:document_codeaction(Uri, Range, [Diag]),
    Changes1 =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => Range,
                        newText => <<"#foobar">>
                    }
                ]
        },
    Changes2 =
        #{
            binary_to_atom(Uri, utf8) =>
                [
                    #{
                        range => DestRange,
                        newText => <<"-record(foo_bar, {}).\n">>
                    }
                ]
        },
    Expected = [
        #{
            edit => #{changes => Changes1},
            kind => <<"quickfix">>,
            title => <<"Did you mean #foobar{}?">>
        },
        #{
            edit => #{changes => Changes2},
            kind => <<"quickfix">>,
            title => <<"Define record foo_bar">>
        }
    ],
    ?assertEqual(Expected, Result),
    ok.
