-module(els_diagnostics_SUITE).

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
    atom_typo/1,
    bound_var_in_pattern/1,
    bound_var_in_pattern_cannot_parse/1,
    compiler/1,
    compiler_with_behaviour/1,
    compiler_with_broken_behaviour/1,
    compiler_with_custom_macros/1,
    compiler_with_parse_transform/1,
    compiler_with_parse_transform_list/1,
    compiler_with_parse_transform_included/1,
    compiler_with_parse_transform_broken/1,
    compiler_with_parse_transform_deps/1,
    compiler_with_parse_transform_error/1,
    compiler_telemetry/1,
    code_path_extra_dirs/1,
    use_long_names/1,
    use_long_names_no_domain/1,
    use_long_names_custom_hostname/1,
    epp_with_nonexistent_macro/1,
    code_reload/1,
    code_reload_sticky_mod/1,
    elvis/1,
    escript/1,
    escript_warnings/1,
    escript_errors/1,
    crossref/1,
    crossref_autoimport/1,
    crossref_autoimport_disabled/1,
    crossref_pseudo_functions/1,
    unused_includes/1,
    unused_includes_compiler_attribute/1,
    unused_includes_broken/1,
    exclude_unused_includes/1,
    unused_macros/1,
    unused_macros_refactorerl/1,
    unused_record_fields/1,
    gradualizer/1,
    eqwalizer/1,
    module_name_check/1,
    module_name_check_whitespace/1,
    edoc_main/1,
    edoc_skip_app_src/1,
    edoc_custom_tags/1
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("els_lsp.hrl").

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
init_per_testcase(TestCase, Config) when
    TestCase =:= code_reload orelse
        TestCase =:= code_reload_sticky_mod
->
    mock_rpc(),
    mock_code_reload_enabled(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when
    TestCase =:= atom_typo
->
    meck:new(els_atom_typo_diagnostics, [passthrough, no_link]),
    meck:expect(els_atom_typo_diagnostics, is_default, 0, true),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when
    TestCase =:= crossref orelse
        TestCase =:= crossref_pseudo_functions orelse
        TestCase =:= crossref_autoimport orelse
        TestCase =:= crossref_autoimport_disabled
->
    meck:new(els_crossref_diagnostics, [passthrough, no_link]),
    meck:expect(els_crossref_diagnostics, is_default, 0, true),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(code_path_extra_dirs, Config) ->
    meck:new(yamerl, [passthrough, no_link]),
    Content = <<"code_path_extra_dirs:\n", "  - \"../code_navigation/*/\"\n">>,
    meck:expect(yamerl, decode_file, 2, fun(_, Opts) ->
        yamerl:decode(Content, Opts)
    end),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(code_path_extra_dirs, Config);
init_per_testcase(use_long_names, Config) ->
    Content =
        <<"runtime:\n", "  use_long_names: true\n", "  cookie: mycookie\n",
            "  node_name: my_node\n", "  domain: test.local">>,
    init_long_names_config(Content, Config);
init_per_testcase(use_long_names_no_domain, Config) ->
    Content =
        <<"runtime:\n", "  use_long_names: true\n", "  cookie: mycookie\n",
            "  node_name: my_node\n">>,
    init_long_names_config(Content, Config);
init_per_testcase(use_long_names_custom_hostname, Config) ->
    Content =
        <<"runtime:\n", "  use_long_names: true\n", "  cookie: mycookie\n",
            "  node_name: my_node\n", "  hostname: 127.0.0.1">>,
    init_long_names_config(Content, Config);
init_per_testcase(exclude_unused_includes = TestCase, Config) ->
    els_mock_diagnostics:setup(),
    NewConfig = els_test_utils:init_per_testcase(TestCase, Config),
    els_config:set(exclude_unused_includes, ["et/include/et.hrl"]),
    NewConfig;
init_per_testcase(TestCase, Config) when TestCase =:= compiler_telemetry ->
    els_mock_diagnostics:setup(),
    mock_compiler_telemetry_enabled(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when TestCase =:= gradualizer ->
    meck:new(els_gradualizer_diagnostics, [passthrough, no_link]),
    meck:expect(els_gradualizer_diagnostics, is_default, 0, true),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when TestCase =:= eqwalizer ->
    meck:new(els_eqwalizer_diagnostics, [passthrough, no_link]),
    meck:expect(els_eqwalizer_diagnostics, is_default, 0, true),
    Diagnostics = [
        els_utils:to_list(
            jsx:encode(#{
                <<"diagnostic">> =>
                    #{
                        <<"code">> => <<"eqwalizer">>,
                        <<"message">> =>
                            <<"Expected: 'ok'\nGot     : 'not_ok'\n">>,
                        <<"range">> =>
                            #{
                                <<"end">> =>
                                    #{
                                        <<"character">> => 10,
                                        <<"line">> => 6
                                    },
                                <<"start">> =>
                                    #{
                                        <<"character">> => 4,
                                        <<"line">> => 6
                                    }
                            },
                        <<"severity">> => 2,
                        <<"source">> => <<"elp">>
                    },
                <<"relative_path">> =>
                    <<"src/diagnostics_eqwalizer.erl">>
            })
        )
    ],
    meck:expect(els_eqwalizer_diagnostics, eqwalize, 2, Diagnostics),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) when
    TestCase =:= edoc_main;
    TestCase =:= edoc_skip_app_src;
    TestCase =:= edoc_custom_tags
->
    meck:new(els_edoc_diagnostics, [passthrough, no_link]),
    meck:expect(els_edoc_diagnostics, is_default, 0, true),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config);
% RefactorErl
init_per_testcase(TestCase, Config) when
    TestCase =:= unused_macros_refactorerl
->
    mock_refactorerl(),
    els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) ->
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) when
    TestCase =:= atom_typo
->
    meck:unload(els_atom_typo_diagnostics),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when
    TestCase =:= code_reload orelse
        TestCase =:= code_reload_sticky_mod
->
    unmock_rpc(),
    unmock_code_reload_enabled(),
    els_test_utils:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config) when
    TestCase =:= crossref orelse
        TestCase =:= crossref_pseudo_functions orelse
        TestCase =:= crossref_autoimport orelse
        TestCase =:= crossref_autoimport_disabled
->
    meck:unload(els_crossref_diagnostics),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when
    TestCase =:= code_path_extra_dirs orelse
        TestCase =:= use_long_names orelse
        TestCase =:= use_long_names_no_domain orelse
        TestCase =:= use_long_names_custom_hostname
->
    meck:unload(yamerl),
    els_test_utils:end_per_testcase(code_path_extra_dirs, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(exclude_unused_includes = TestCase, Config) ->
    els_config:set(exclude_unused_includes, []),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when TestCase =:= compiler_telemetry ->
    unmock_compiler_telemetry_enabled(),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when TestCase =:= gradualizer ->
    meck:unload(els_gradualizer_diagnostics),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when TestCase =:= eqwalizer ->
    meck:unload(els_eqwalizer_diagnostics),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when
    TestCase =:= edoc_main;
    TestCase =:= edoc_skip_app_src;
    TestCase =:= edoc_custom_tags
->
    meck:unload(els_edoc_diagnostics),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) when
    TestCase =:= unused_macros_refactorerl
->
    unmock_refactoerl(),
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok;
end_per_testcase(TestCase, Config) ->
    els_test_utils:end_per_testcase(TestCase, Config),
    els_mock_diagnostics:teardown(),
    ok.

-spec init_long_names_config(binary(), config()) -> config().
init_long_names_config(Content, Config) ->
    meck:new(yamerl, [passthrough, no_link]),
    meck:expect(yamerl, decode_file, 2, fun(_, Opts) ->
        yamerl:decode(Content, Opts)
    end),
    els_mock_diagnostics:setup(),
    els_test_utils:init_per_testcase(code_path_extra_dirs, Config).

% RefactorErl

%%==============================================================================
%% Testcases
%%==============================================================================
-spec atom_typo(config()) -> ok.
atom_typo(_Config) ->
    Path = src_path("atom_typo.erl"),
    Source = <<"AtomTypo">>,
    Errors = [],
    Warnings = [
        #{
            message => <<"Atom typo? Did you mean: true">>,
            range => {{5, 2}, {5, 6}}
        },
        #{
            message => <<"Atom typo? Did you mean: false">>,
            range => {{6, 2}, {6, 8}}
        },
        #{
            message => <<"Atom typo? Did you mean: false">>,
            range => {{7, 2}, {7, 7}}
        },
        #{
            message => <<"Atom typo? Did you mean: undefined">>,
            range => {{8, 2}, {8, 11}}
        },
        #{
            message => <<"Atom typo? Did you mean: undefined">>,
            range => {{9, 2}, {9, 10}}
        },
        #{
            message => <<"Atom typo? Did you mean: error">>,
            range => {{10, 2}, {10, 8}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec bound_var_in_pattern(config()) -> ok.
bound_var_in_pattern(_Config) ->
    Path = src_path("diagnostics_bound_var_in_pattern.erl"),
    Source = <<"BoundVarInPattern">>,
    Errors = [],
    Warnings = [],
    Hints = [
        #{
            message => <<"Bound variable in pattern: Var1">>,
            range => {{5, 2}, {5, 6}}
        },
        #{
            message => <<"Bound variable in pattern: Var2">>,
            range => {{9, 9}, {9, 13}}
        },
        #{
            message => <<"Bound variable in pattern: Var4">>,
            range => {{17, 8}, {17, 12}}
        },
        #{
            message => <<"Bound variable in pattern: Var3">>,
            range => {{15, 10}, {15, 14}}
        },
        #{
            message => <<"Bound variable in pattern: Var5">>,
            range => {{23, 6}, {23, 10}}
        }
        %% erl_syntax_lib:annotate_bindings does not handle named funs
        %% correctly
        %% , #{ message => <<"Bound variable in pattern: New">>
        %%    , range => {{28, 6}, {28, 9}}}
        %% , #{ message => <<"Bound variable in pattern: F">>
        %%    , range => {{29, 6}, {29, 9}}}
    ],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec bound_var_in_pattern_cannot_parse(config()) -> ok.
bound_var_in_pattern_cannot_parse(_Config) ->
    Path = src_path("diagnostics_bound_var_in_pattern_cannot_parse.erl"),
    Source = <<"BoundVarInPattern">>,
    Errors = [],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler(config()) -> ok.
compiler(_Config) ->
    Path = src_path("diagnostics.erl"),
    Source = <<"Compiler">>,
    Errors = [
        #{
            code => <<"L0000">>,
            message => <<"Issue in included file (1): bad attribute">>,
            range => {{3, 0}, {3, 35}}
        },
        #{
            code => <<"L0000">>,
            message => <<"Issue in included file (3): bad attribute">>,
            range => {{3, 0}, {3, 35}}
        },
        #{
            code => <<"L1295">>,
            message => <<"type undefined_type() undefined">>,
            range => {{5, 30}, {5, 44}}
        }
    ],
    Warnings = [
        #{
            code => <<"L1230">>,
            message => <<"function main/1 is unused">>,
            range => {{6, 0}, {6, 4}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_behaviour(config()) -> ok.
compiler_with_behaviour(_Config) ->
    Path = src_path("diagnostics_behaviour_impl.erl"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1284">>,
            message =>
                <<
                    "undefined callback function one/0 "
                    "(behaviour 'diagnostics_behaviour')"
                >>,
            range => {{2, 0}, {2, 34}}
        },
        #{
            code => <<"L1284">>,
            message =>
                <<
                    "undefined callback function two/0 "
                    "(behaviour 'diagnostics_behaviour')"
                >>,
            range => {{2, 0}, {2, 34}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

%% Testing #614
-spec compiler_with_broken_behaviour(config()) -> ok.
compiler_with_broken_behaviour(_Config) ->
    Path = src_path("code_navigation.erl"),
    {ok, Session} = els_test:start_session(Path),
    Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
    els_test:assert_contains(
        #{
            code => <<"L0000">>,
            message => <<"Issue in included file (5): syntax error before: ">>,
            range => {{2, 0}, {2, 24}}
        },
        Diagnostics
    ).

-spec compiler_with_custom_macros(config()) -> ok.
compiler_with_custom_macros(_Config) ->
    %% This test uses priv/code_navigation/erlang_ls.config to define
    %% some macros.
    Path = src_path("diagnostics_macros.erl"),
    Source = <<"Compiler">>,
    Errors =
        case els_test:compiler_returns_column_numbers() of
            true ->
                %% diagnostic_macro has a spec with no '.' at the end
                %% which causes the poi for the spec to becomes the
                %% entire spec + function. So this range here is 8
                %% lines long.
                [
                    #{
                        code => <<"E1507">>,
                        message => <<"undefined macro 'UNDEFINED'">>,
                        range => {{2, 0}, {10, 6}}
                    }
                ];
            false ->
                [
                    #{
                        code => <<"E1507">>,
                        message => <<"undefined macro 'UNDEFINED'">>,
                        range => {{8, 0}, {9, 0}}
                    }
                ]
        end,
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_parse_transform(config()) -> ok.
compiler_with_parse_transform(_Config) ->
    _ = code:delete(diagnostics_parse_transform),
    _ = code:purge(diagnostics_parse_transform),
    Path = src_path("diagnostics_parse_transform_usage.erl"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1268">>,
            message => <<"variable 'Args' is unused">>,
            range => {{6, 5}, {6, 9}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_parse_transform_list(config()) -> ok.
compiler_with_parse_transform_list(_Config) ->
    _ = code:delete(diagnostics_parse_transform),
    _ = code:purge(diagnostics_parse_transform),
    Path = src_path("diagnostics_parse_transform_usage_list.erl"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1268">>,
            message => <<"variable 'Args' is unused">>,
            range => {{6, 5}, {6, 9}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_parse_transform_included(config()) -> ok.
compiler_with_parse_transform_included(_Config) ->
    _ = code:delete(diagnostics_parse_transform),
    _ = code:purge(diagnostics_parse_transform),
    Path = src_path("diagnostics_parse_transform_usage_included.erl"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1268">>,
            message => <<"variable 'Args' is unused">>,
            range => {{6, 5}, {6, 9}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_parse_transform_broken(config()) -> ok.
compiler_with_parse_transform_broken(_Config) ->
    Path = src_path("diagnostics_parse_transform_usage_broken.erl"),
    Source = <<"Compiler">>,
    Errors =
        [
            #{
                code => <<"L0000">>,
                message => <<"Issue in included file (10): syntax error before: ">>,
                range => {{4, 27}, {4, 61}}
            },
            #{
                code => <<"C1008">>,
                message => <<
                    "undefined parse transform "
                    "'diagnostics_parse_transform_broken'"
                >>,
                range => {{0, 0}, {1, 0}}
            }
        ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_with_parse_transform_deps(config()) -> ok.
compiler_with_parse_transform_deps(_Config) ->
    Path = src_path("diagnostics_parse_transform_deps_a.erl"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1230">>,
            message => <<"function unused/0 is unused">>,
            range => {{4, 0}, {4, 6}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

%% Issue 1140
-spec compiler_with_parse_transform_error(config()) -> ok.
compiler_with_parse_transform_error(_Config) ->
    Path = src_path("diagnostics_parse_transform_error.erl"),
    Source = <<"Compiler">>,
    Errors = [
        #{
            code => <<"my_parse_transform">>,
            message => <<"custom_description">>,
            range => {{41, 0}, {42, 0}}
        }
    ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec compiler_telemetry(config()) -> ok.
compiler_telemetry(Config) ->
    Path = src_path("diagnostics.erl"),
    Source = <<"Compiler">>,
    Errors = [
        #{
            code => <<"L0000">>,
            message => <<"Issue in included file (1): bad attribute">>,
            range => {{3, 0}, {3, 35}}
        },
        #{
            code => <<"L0000">>,
            message => <<"Issue in included file (3): bad attribute">>,
            range => {{3, 0}, {3, 35}}
        },
        #{
            code => <<"L1295">>,
            message => <<"type undefined_type() undefined">>,
            range => {{5, 30}, {5, 44}}
        }
    ],
    Warnings = [
        #{
            code => <<"L1230">>,
            message => <<"function main/1 is unused">>,
            range => {{6, 0}, {6, 4}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints),
    Telemetry = wait_for_compiler_telemetry(),
    #{
        type := Type,
        uri := UriT,
        diagnostics := DiagnosticsCodes
    } = Telemetry,
    ?assertEqual(<<"erlang-diagnostic-codes">>, Type),
    Uri = ?config(diagnostics_uri, Config),
    ?assertEqual(Uri, UriT),
    ?assertEqual(
        [<<"L1230">>, <<"L0000">>, <<"L0000">>, <<"L1295">>],
        DiagnosticsCodes
    ),
    ok.

-spec code_path_extra_dirs(config()) -> ok.
code_path_extra_dirs(_Config) ->
    RootPath = binary_to_list(els_test_utils:root_path()),
    Dirs = [
        AbsDir
     || Dir <- filelib:wildcard("*", RootPath),
        filelib:is_dir(AbsDir = filename:absname(Dir, RootPath))
    ],
    ?assertMatch(true, lists:all(fun(Elem) -> code:del_path(Elem) end, Dirs)),
    ok.

-spec use_long_names(config()) -> ok.
use_long_names(_Config) ->
    HostName = els_config_runtime:get_hostname(),
    NodeName =
        "my_node@" ++
            HostName ++ "." ++
            els_config_runtime:get_domain(),
    Node = list_to_atom(NodeName),
    ?assertMatch(Node, els_config_runtime:get_node_name()),
    ok.

-spec use_long_names_no_domain(config()) -> ok.
use_long_names_no_domain(_Config) ->
    HostName = els_config_runtime:get_hostname(),
    NodeName =
        "my_node@" ++ HostName,
    Node = list_to_atom(NodeName),
    ?assertMatch(Node, els_config_runtime:get_node_name()),
    ok.

-spec use_long_names_custom_hostname(config()) -> ok.
use_long_names_custom_hostname(_Config) ->
    HostName = els_config_runtime:get_hostname(),
    NodeName = "my_node@127.0.0.1",
    Node = list_to_atom(NodeName),
    ?assertMatch(HostName, "127.0.0.1"),
    ?assertMatch(Node, els_config_runtime:get_node_name()),
    ok.

-spec epp_with_nonexistent_macro(config()) -> ok.
epp_with_nonexistent_macro(_Config) ->
    Path = include_path("nonexistent_macro.hrl"),
    Source = <<"Compiler">>,
    Errors = [
        #{
            code => <<"E1516">>,
            message => <<"can't find include file \"nonexisten-file.hrl\"">>,
            range => {{2, 0}, {3, 0}}
        },
        #{
            code => <<"E1507">>,
            message => <<"undefined macro 'MODULE'">>,
            range => {{4, 0}, {5, 0}}
        },
        #{
            code => <<"E1522">>,
            message => <<
                "-error(\"including nonexistent_macro.hrl "
                "is not allowed\")."
            >>,
            range => {{6, 0}, {7, 0}}
        }
    ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec elvis(config()) -> ok.
elvis(_Config) ->
    {ok, Cwd} = file:get_cwd(),
    RootPath = els_test_utils:root_path(),
    try
        file:set_cwd(RootPath),
        Path = src_path("elvis_diagnostics.erl"),
        Source = <<"Elvis">>,
        Errors = [],
        Warnings = [
            #{
                code => operator_spaces,
                message => <<"Missing space right \",\" on line 6">>,
                range => {{5, 0}, {6, 0}},
                relatedInformation => []
            },
            #{
                code => operator_spaces,
                message => <<"Missing space right \",\" on line 7">>,
                range => {{6, 0}, {7, 0}},
                relatedInformation => []
            }
        ],
        Hints = [],
        els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints)
    catch
        _Err ->
            file:set_cwd(Cwd)
    end,
    ok.

-spec escript(config()) -> ok.
escript(_Config) ->
    Path = src_path("diagnostics.escript"),
    Source = <<"Compiler">>,
    els_test:run_diagnostics_test(Path, Source, [], [], []).

-spec escript_warnings(config()) -> ok.
escript_warnings(_Config) ->
    Path = src_path("diagnostics_warnings.escript"),
    Source = <<"Compiler">>,
    Errors = [],
    Warnings = [
        #{
            code => <<"L1230">>,
            message => <<"function unused/0 is unused">>,
            range => {{23, 0}, {24, 0}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec escript_errors(config()) -> ok.
escript_errors(_Config) ->
    Path = src_path("diagnostics_errors.escript"),
    Source = <<"Compiler">>,
    Errors = [
        #{
            code => <<"P1711">>,
            message => <<"syntax error before: tion_with_error">>,
            range => {{23, 0}, {24, 0}}
        }
    ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec code_reload(config()) -> ok.
code_reload(Config) ->
    Uri = ?config(diagnostics_uri, Config),
    Module = els_uri:module(Uri),
    ok = els_compiler_diagnostics:on_complete(Uri, []),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    ?assert(meck:called(rpc, call, [NodeName, c, c, [Module]])),
    ok.

-spec code_reload_sticky_mod(config()) -> ok.
code_reload_sticky_mod(Config) ->
    Uri = ?config(diagnostics_uri, Config),
    Module = els_uri:module(Uri),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    meck:expect(
        rpc,
        call,
        fun
            (PNode, code, is_sticky, [_]) when PNode =:= NodeName ->
                true;
            (Node, Mod, Fun, Args) ->
                meck:passthrough([Node, Mod, Fun, Args])
        end
    ),
    ok = els_compiler_diagnostics:on_complete(Uri, []),
    ?assert(meck:called(rpc, call, [NodeName, code, is_sticky, [Module]])),
    ?assertNot(meck:called(rpc, call, [NodeName, c, c, [Module]])),
    ok.

-spec crossref(config()) -> ok.
crossref(_Config) ->
    Path = src_path("diagnostics_xref.erl"),
    Source = <<"CrossRef">>,
    Errors =
        [
            #{
                message => <<"Cannot find definition for function non_existing/0">>,
                range => {{6, 2}, {6, 14}}
            },
            #{
                message => <<"Cannot find definition for function lists:map/3">>,
                range => {{5, 2}, {5, 11}}
            }
        ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

%% #641
-spec crossref_pseudo_functions(config()) -> ok.
crossref_pseudo_functions(_Config) ->
    Path = src_path("diagnostics_xref_pseudo.erl"),
    Errors =
        [
            #{
                message =>
                    <<
                        "Cannot find definition for function "
                        "unknown_module:nonexistent/0"
                    >>,
                range => {{34, 2}, {34, 28}}
            },
            #{
                message =>
                    <<
                        "Cannot find definition for function "
                        "unknown_module:module_info/1"
                    >>,
                range => {{13, 2}, {13, 28}}
            },
            #{
                message =>
                    <<
                        "Cannot find definition for function "
                        "unknown_module:module_info/0"
                    >>,
                range => {{12, 2}, {12, 28}}
            }
        ],
    els_test:run_diagnostics_test(Path, <<"CrossRef">>, Errors, [], []).

%% #860
-spec crossref_autoimport(config()) -> ok.
crossref_autoimport(_Config) ->
    %% This testcase cannot be run from an Erlang source tree version,
    %% it needs a released version.
    Path = src_path("diagnostics_autoimport.erl"),
    els_test:run_diagnostics_test(Path, <<"CrossRef">>, [], [], []).

%% #860
-spec crossref_autoimport_disabled(config()) -> ok.
crossref_autoimport_disabled(_Config) ->
    %% This testcase cannot be run from an Erlang source tree version,
    %% it needs a released version.
    Path = src_path("diagnostics_autoimport_disabled.erl"),
    els_test:run_diagnostics_test(Path, <<"CrossRef">>, [], [], []).

-spec unused_includes(config()) -> ok.
unused_includes(_Config) ->
    Path = src_path("diagnostics_unused_includes.erl"),
    Source = <<"UnusedIncludes">>,
    Errors = [],
    {ok, FileName} = els_utils:find_header(
        els_utils:filename_to_atom("et/include/et.hrl")
    ),
    Warnings = [
        #{
            message => <<"Unused file: et.hrl">>,
            range => {{3, 0}, {3, 34}},
            data => FileName
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec unused_includes_compiler_attribute(config()) -> ok.
unused_includes_compiler_attribute(_Config) ->
    Path = src_path("diagnostics_unused_includes_compiler_attribute.erl"),
    Source = <<"UnusedIncludes">>,
    Errors = [],
    {ok, FileName} = els_utils:find_header(
        els_utils:filename_to_atom("kernel/include/file.hrl")
    ),
    Warnings = [
        #{
            message => <<"Unused file: file.hrl">>,
            range => {{3, 0}, {3, 40}},
            data => FileName
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec unused_includes_broken(config()) -> ok.
unused_includes_broken(_Config) ->
    Path = src_path("diagnostics_unused_includes_broken.erl"),
    Source = <<"UnusedIncludes">>,
    Errors = [],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec exclude_unused_includes(config()) -> ok.
exclude_unused_includes(_Config) ->
    Path = src_path("diagnostics_unused_includes.erl"),
    Source = <<"UnusedIncludes">>,
    Errors = [],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec unused_macros(config()) -> ok.
unused_macros(_Config) ->
    Path = src_path("diagnostics_unused_macros.erl"),
    Source = <<"UnusedMacros">>,
    Errors = [],
    Warnings = [
        #{
            message => <<"Unused macro: UNUSED_MACRO">>,
            range => {{5, 8}, {5, 20}}
        },
        #{
            message => <<"Unused macro: UNUSED_MACRO_WITH_ARG/1">>,
            range => {{6, 8}, {6, 29}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec unused_record_fields(config()) -> ok.
unused_record_fields(_Config) ->
    Path = src_path("diagnostics_unused_record_fields.erl"),
    Source = <<"UnusedRecordFields">>,
    Errors = [],
    Warnings =
        [
            #{
                message => <<"Unused record field: #unused_field.field_d">>,
                range => {{5, 32}, {5, 39}}
            }
        ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec gradualizer(config()) -> ok.
gradualizer(_Config) ->
    Path = src_path("diagnostics_gradualizer.erl"),
    Source = <<"Gradualizer">>,
    Errors = [],
    Warnings = [
        #{
            message =>
                <<
                    "The variable N is expected to have type integer() "
                    "but it has type false | true\n"
                >>,
            range => {{10, 0}, {11, 0}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec eqwalizer(config()) -> ok.
eqwalizer(_Config) ->
    Path = src_path("diagnostics_eqwalizer.erl"),
    Source = <<"EqWAlizer">>,
    Errors = [],
    Warnings = [
        #{
            message => <<"Expected: 'ok'\nGot     : 'not_ok'\n">>,
            range => {{6, 4}, {6, 10}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec module_name_check(config()) -> ok.
module_name_check(_Config) ->
    Path = src_path("diagnostics_module_name_check.erl"),
    Source = <<"Compiler (via Erlang LS)">>,
    Errors = [
        #{
            message =>
                <<
                    "Module name 'module_name_check' does not match "
                    "file name 'diagnostics_module_name_check'"
                >>,
            range => {{0, 8}, {0, 25}}
        }
    ],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec module_name_check_whitespace(config()) -> ok.
module_name_check_whitespace(_Config) ->
    Path = src_path("diagnostics module name check.erl"),
    Source = <<"Compiler (via Erlang LS)">>,
    Errors = [],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec edoc_main(config()) -> ok.
edoc_main(_Config) ->
    Path = src_path("edoc_diagnostics.erl"),
    Source = <<"Edoc">>,
    Errors = [
        #{
            message => <<"`-quote ended unexpectedly at line 13">>,
            range => {{12, 0}, {13, 0}}
        }
    ],
    Warnings = [
        #{
            message =>
                <<"tag @mydoc not recognized.">>,
            range => {{4, 0}, {5, 0}}
        },
        #{
            message =>
                <<"tag @docc not recognized.">>,
            range => {{8, 0}, {9, 0}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec edoc_skip_app_src(config()) -> ok.
edoc_skip_app_src(_Config) ->
    Path = src_path("code_navigation.app.src"),
    Source = <<"Edoc">>,
    Errors = [],
    Warnings = [],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

-spec edoc_custom_tags(config()) -> ok.
edoc_custom_tags(_Config) ->
    %% Custom tags are defined in priv/code_navigation/erlang_ls.config
    Path = src_path("edoc_diagnostics_custom_tags.erl"),
    Source = <<"Edoc">>,
    Errors = [],
    Warnings = [
        #{
            message =>
                <<"tag @docc not recognized.">>,
            range => {{9, 0}, {10, 0}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

% RefactorErl test cases
-spec unused_macros_refactorerl(config()) -> ok.
unused_macros_refactorerl(_Config) ->
    Path = src_path("diagnostics_unused_macros.erl"),
    Source = <<"RefactorErl">>,
    Errors = [],
    Warnings = [
        #{
            message => <<"Unused macro: UNUSED_MACRO">>,
            range => {{5, 0}, {5, 35}}
        },
        #{
            message => <<"Unused macro: UNUSED_MACRO_WITH_ARG">>,
            range => {{6, 0}, {6, 36}}
        }
    ],
    Hints = [],
    els_test:run_diagnostics_test(Path, Source, Errors, Warnings, Hints).

%%==============================================================================
%% Internal Functions
%%==============================================================================

mock_rpc() ->
    meck:new(rpc, [passthrough, no_link, unstick]),
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("fakenode@" ++ HostName),
    meck:expect(
        rpc,
        call,
        fun
            (PNode, c, c, [Module]) when PNode =:= NodeName ->
                {ok, Module};
            (Node, Mod, Fun, Args) ->
                meck:passthrough([Node, Mod, Fun, Args])
        end
    ).

unmock_rpc() ->
    meck:unload(rpc).

mock_code_reload_enabled() ->
    meck:new(els_config, [passthrough, no_link]),
    meck:expect(
        els_config,
        get,
        fun
            (code_reload) ->
                {ok, HostName} = inet:gethostname(),
                #{"node" => "fakenode@" ++ HostName};
            (Key) ->
                meck:passthrough([Key])
        end
    ).

unmock_code_reload_enabled() ->
    meck:unload(els_config).

mock_compiler_telemetry_enabled() ->
    meck:new(els_config, [passthrough, no_link]),
    meck:expect(
        els_config,
        get,
        fun
            (compiler_telemetry_enabled) ->
                true;
            (Key) ->
                meck:passthrough([Key])
        end
    ),
    Self = self(),
    meck:expect(
        els_server,
        send_notification,
        fun
            (<<"telemetry/event">> = Method, Params) ->
                Self ! {on_complete_telemetry, Params},
                meck:passthrough([Method, Params]);
            (M, P) ->
                meck:passthrough([M, P])
        end
    ),
    ok.

-spec wait_for_compiler_telemetry() -> {uri(), [els_diagnostics:diagnostic()]}.
wait_for_compiler_telemetry() ->
    receive
        {on_complete_telemetry, #{type := <<"erlang-diagnostic-codes">>} = Params} ->
            Params
    end.

unmock_compiler_telemetry_enabled() ->
    meck:unload(els_config),
    meck:unload(els_server).

src_path(Module) ->
    filename:join(["code_navigation", "src", Module]).

include_path(Header) ->
    filename:join(["code_navigation", "include", Header]).

% Mock RefactorErl utils
mock_refactorerl() ->
    {ok, HostName} = inet:gethostname(),
    NodeName = list_to_atom("referl_fake@" ++ HostName),

    meck:new(els_refactorerl_utils, [passthrough, no_link, unstick]),
    meck:expect(
        els_refactorerl_utils,
        run_diagnostics,
        2,
        [
            {
                #{
                    'end' => #{character => 35, line => 5},
                    start => #{character => 0, line => 5}
                },
                <<"Unused macro: UNUSED_MACRO">>
            },
            {
                #{
                    'end' => #{character => 36, line => 6},
                    start => #{character => 0, line => 6}
                },
                <<"Unused macro: UNUSED_MACRO_WITH_ARG">>
            }
        ]
    ),
    meck:expect(els_refactorerl_utils, referl_node, 0, {ok, NodeName}),
    meck:expect(els_refactorerl_utils, add, 1, ok),
    meck:expect(els_refactorerl_utils, source_name, 0, <<"RefactorErl">>),

    meck:new(els_refactorerl_diagnostics, [passthrough, no_link, unstick]),
    meck:expect(els_refactorerl_diagnostics, is_default, 0, true).

unmock_refactoerl() ->
    meck:unload(els_refactorerl_diagnostics),
    meck:unload(els_refactorerl_utils).
