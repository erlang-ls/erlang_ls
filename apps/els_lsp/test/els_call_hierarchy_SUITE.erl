-module(els_call_hierarchy_SUITE).

-include("els_lsp.hrl").

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
    incoming_calls/1,
    outgoing_calls/1
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
%% Testcases
%%==============================================================================
-spec incoming_calls(config()) -> ok.
incoming_calls(Config) ->
    UriA = ?config(call_hierarchy_a_uri, Config),
    UriB = ?config(call_hierarchy_b_uri, Config),
    #{result := PrepareResult} =
        els_client:preparecallhierarchy(UriA, _Line = 11, _Char = 6),
    Data = els_utils:base64_encode_term(
        #{
            poi =>
                #{
                    data =>
                        #{
                            args => [
                                #{
                                    index => 1,
                                    name => "N",
                                    range => #{
                                        from => {9, 12},
                                        to => {9, 13}
                                    }
                                }
                            ],

                            wrapping_range => #{
                                from => {7, 1},
                                to => {17, 0}
                            },
                            symbol_range => #{from => {7, 1}, to => {16, 19}},
                            folding_range => #{
                                from => {7, ?END_OF_LINE},
                                to => {16, ?END_OF_LINE}
                            }
                        },
                    id => {function_a, 1},
                    kind => function,
                    range => #{
                        from => {7, 1},
                        to => {7, 11}
                    }
                }
        }
    ),
    Item = #{
        data => Data,
        detail => <<"call_hierarchy_a [L7]">>,
        kind => ?SYMBOLKIND_FUNCTION,
        name => <<"function_a/1">>,
        range =>
            #{
                'end' => #{character => 10, line => 6},
                start => #{character => 0, line => 6}
            },
        selectionRange =>
            #{
                'end' => #{character => 10, line => 6},
                start => #{character => 0, line => 6}
            },
        uri => UriA
    },
    ?assertEqual([Item], PrepareResult),
    #{result := Result} = els_client:callhierarchy_incomingcalls(Item),
    Calls = [
        #{
            from =>
                #{
                    data =>
                        els_utils:base64_encode_term(
                            #{
                                poi =>
                                    #{
                                        data =>
                                            #{
                                                args => [
                                                    #{
                                                        index => 1,
                                                        name => "N",
                                                        range => #{
                                                            from => {9, 12},
                                                            to => {9, 13}
                                                        }
                                                    }
                                                ],
                                                wrapping_range =>
                                                    #{
                                                        from => {7, 1},
                                                        to => {14, 0}
                                                    },

                                                symbol_range => #{from => {7, 1}, to => {13, 19}},
                                                folding_range =>
                                                    #{
                                                        from => {7, ?END_OF_LINE},
                                                        to => {13, ?END_OF_LINE}
                                                    }
                                            },
                                        id => {function_a, 1},
                                        kind => function,
                                        range => #{from => {7, 1}, to => {7, 11}}
                                    }
                            }
                        ),
                    detail => <<"call_hierarchy_b [L11]">>,
                    kind => 12,
                    name => <<"function_a/1">>,
                    range =>
                        #{
                            'end' => #{character => 29, line => 10},
                            start => #{character => 2, line => 10}
                        },
                    selectionRange =>
                        #{
                            'end' => #{character => 29, line => 10},
                            start => #{character => 2, line => 10}
                        },
                    uri => UriB
                },
            fromRanges =>
                [
                    #{
                        'end' => #{character => 29, line => 10},
                        start => #{character => 2, line => 10}
                    }
                ]
        },
        #{
            from =>
                #{
                    data =>
                        els_utils:base64_encode_term(
                            #{
                                poi =>
                                    #{
                                        data =>
                                            #{
                                                args => [
                                                    #{
                                                        index => 1,
                                                        name => "N",
                                                        range => #{
                                                            from => {9, 12},
                                                            to => {9, 13}
                                                        }
                                                    }
                                                ],
                                                wrapping_range =>
                                                    #{
                                                        from => {7, 1},
                                                        to => {17, 0}
                                                    },
                                                symbol_range => #{from => {7, 1}, to => {16, 19}},
                                                folding_range =>
                                                    #{
                                                        from => {7, ?END_OF_LINE},
                                                        to => {16, ?END_OF_LINE}
                                                    }
                                            },
                                        id => {function_a, 1},
                                        kind => function,
                                        range => #{
                                            from => {7, 1},
                                            to => {7, 11}
                                        }
                                    }
                            }
                        ),
                    detail => <<"call_hierarchy_a [L16]">>,
                    kind => 12,
                    name => <<"function_a/1">>,
                    range =>
                        #{
                            'end' => #{
                                character => 12,
                                line => 15
                            },
                            start => #{
                                character => 2,
                                line => 15
                            }
                        },
                    selectionRange =>
                        #{
                            'end' => #{
                                character => 12,
                                line => 15
                            },
                            start => #{
                                character => 2,
                                line => 15
                            }
                        },
                    uri => UriA
                },
            fromRanges =>
                [
                    #{
                        'end' => #{character => 12, line => 15},
                        start => #{character => 2, line => 15}
                    }
                ]
        }
    ],
    lists:map(
        fun(Call) ->
            case lists:member(Call, Result) of
                true ->
                    ct:comment("Call found: ~p", [Call]);
                false ->
                    ct:fail("Call not found: ~p", [Call])
            end
        end,
        Calls
    ),
    ?assertEqual(length(Calls), length(Result)).

-spec outgoing_calls(config()) -> ok.
outgoing_calls(Config) ->
    UriA = ?config(call_hierarchy_a_uri, Config),
    #{result := PrepareResult} =
        els_client:preparecallhierarchy(UriA, _Line = 9, _Char = 6),
    Data = els_utils:base64_encode_term(
        #{
            poi =>
                #{
                    data => #{
                        args => [
                            #{
                                index => 1,
                                name => "N",
                                range => #{from => {9, 12}, to => {9, 13}}
                            }
                        ],
                        wrapping_range => #{
                            from => {7, 1},
                            to => {17, 0}
                        },
                        symbol_range => #{from => {7, 1}, to => {16, 19}},
                        folding_range => #{
                            from => {7, ?END_OF_LINE},
                            to => {16, ?END_OF_LINE}
                        }
                    },
                    id => {function_a, 1},
                    kind => function,
                    range => #{
                        from => {7, 1},
                        to => {7, 11}
                    }
                }
        }
    ),
    Item = #{
        data => Data,
        detail => <<"call_hierarchy_a [L7]">>,
        kind => ?SYMBOLKIND_FUNCTION,
        name => <<"function_a/1">>,
        range =>
            #{
                'end' => #{character => 10, line => 6},
                start => #{character => 0, line => 6}
            },
        selectionRange =>
            #{
                'end' => #{character => 10, line => 6},
                start => #{character => 0, line => 6}
            },
        uri => UriA
    },
    ?assertEqual([Item], PrepareResult),
    #{result := Result} = els_client:callhierarchy_outgoingcalls(Item),
    POIs = [els_utils:base64_decode_term(D) || #{to := #{data := D}} <- Result],
    ?assertEqual(
        [
            #{
                poi =>
                    #{
                        data =>
                            #{
                                args => [
                                    #{
                                        index => 1,
                                        name => "N",
                                        range => #{from => {9, 12}, to => {9, 13}}
                                    }
                                ],
                                wrapping_range => #{
                                    from => {7, 1},
                                    to => {17, 0}
                                },
                                symbol_range => #{from => {7, 1}, to => {16, 19}},
                                folding_range => #{
                                    from => {7, ?END_OF_LINE},
                                    to => {16, ?END_OF_LINE}
                                }
                            },
                        id => {function_a, 1},
                        kind => function,
                        range => #{
                            from => {7, 1},
                            to => {7, 11}
                        }
                    }
            },
            #{
                poi =>
                    #{
                        data =>
                            #{
                                args => [],
                                wrapping_range => #{
                                    from => {18, 1},
                                    to => {20, 0}
                                },
                                symbol_range => #{from => {18, 1}, to => {19, 6}},
                                folding_range => #{
                                    from => {18, ?END_OF_LINE},
                                    to => {19, ?END_OF_LINE}
                                }
                            },
                        id => {function_b, 0},
                        kind => function,
                        range => #{
                            from => {18, 1},
                            to => {18, 11}
                        }
                    }
            },
            #{
                poi =>
                    #{
                        data =>
                            #{
                                args => [
                                    #{
                                        index => 1,
                                        name => "N",
                                        range => #{from => {9, 12}, to => {9, 13}}
                                    }
                                ],
                                wrapping_range => #{
                                    from => {7, 1},
                                    to => {14, 0}
                                },
                                symbol_range => #{from => {7, 1}, to => {13, 19}},
                                folding_range => #{
                                    from => {7, ?END_OF_LINE},
                                    to => {13, ?END_OF_LINE}
                                }
                            },
                        id => {function_a, 1},
                        kind => function,
                        range => #{
                            from => {7, 1},
                            to => {7, 11}
                        }
                    }
            },
            #{
                poi =>
                    #{
                        data =>
                            #{
                                args => [],
                                wrapping_range => #{
                                    from => {18, 1},
                                    to => {20, 0}
                                },
                                symbol_range => #{from => {18, 1}, to => {19, 6}},
                                folding_range => #{
                                    from => {18, ?END_OF_LINE},
                                    to => {19, ?END_OF_LINE}
                                }
                            },
                        id => {function_b, 0},
                        kind => function,
                        range => #{
                            from => {18, 1},
                            to => {18, 11}
                        }
                    }
            }
        ],
        POIs
    ).
