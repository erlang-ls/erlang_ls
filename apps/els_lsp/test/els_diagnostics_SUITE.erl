%% Fixcolumns
%% Refactor like crossref to avoid repetition
%% Dialyzer diagnostics type
%% Sort diagnostics
-module(els_diagnostics_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ bound_var_in_pattern/1
        , compiler/1
        , compiler_with_behaviour/1
        , compiler_with_broken_behaviour/1
        , compiler_with_custom_macros/1
        , compiler_with_parse_transform/1
        , compiler_with_parse_transform_list/1
        , compiler_with_parse_transform_included/1
        , compiler_with_parse_transform_broken/1
        , compiler_with_parse_transform_deps/1
        , compiler_telemetry/1
        , code_path_extra_dirs/1
        , use_long_names/1
        , epp_with_nonexistent_macro/1
        , code_reload/1
        , code_reload_sticky_mod/1
        , elvis/1
        , escript/1
        , escript_warnings/1
        , escript_errors/1
        , crossref/1
        , crossref_autoimport/1
        , crossref_autoimport_disabled/1
        , crossref_pseudo_functions/1
        , unused_includes/1
        , unused_includes_compiler_attribute/1
        , exclude_unused_includes/1
        , unused_macros/1
        , unused_record_fields/1
        , gradualizer/1
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
  %% If epp:open/5 is exported we know that columns are not
  %% returned by the compiler warnings and errors.
  %% Should find a better heuristic for this.
  [{columns, not erlang:function_exported(epp, open, 5)} |
   els_test_utils:init_per_suite(Config)].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  els_test_utils:end_per_suite(Config).

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) when TestCase =:= code_reload orelse
                                         TestCase =:= code_reload_sticky_mod ->
  mock_rpc(),
  mock_code_reload_enabled(),
  els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config)
     when TestCase =:= crossref orelse
          TestCase =:= crossref_pseudo_functions orelse
          TestCase =:= crossref_autoimport orelse
          TestCase =:= crossref_autoimport_disabled ->
  meck:new(els_crossref_diagnostics, [passthrough, no_link]),
  meck:expect(els_crossref_diagnostics, is_default, 0, true),
  els_mock_diagnostics:setup(),
  els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(code_path_extra_dirs, Config) ->
  meck:new(yamerl, [passthrough, no_link]),
  Content = <<"code_path_extra_dirs:\n",
              "  - \"../code_navigation/*/\"\n">>,
  meck:expect(yamerl, decode_file, 2, fun(_, Opts) ->
                                        yamerl:decode(Content, Opts)
                                      end),
  els_mock_diagnostics:setup(),
  els_test_utils:init_per_testcase(code_path_extra_dirs, Config);
init_per_testcase(use_long_names, Config) ->
  meck:new(yamerl, [passthrough, no_link]),
  Content = <<"runtime:\n",
              "  use_long_names: true\n",
              "  cookie: mycookie\n",
              "  node_name: my_node\n">>,
  meck:expect(yamerl, decode_file, 2, fun(_, Opts) ->
                                        yamerl:decode(Content, Opts)
                                      end),
  els_mock_diagnostics:setup(),
  els_test_utils:init_per_testcase(code_path_extra_dirs, Config);
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
init_per_testcase(TestCase, Config) ->
  els_mock_diagnostics:setup(),
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) when TestCase =:= code_reload orelse
                                        TestCase =:= code_reload_sticky_mod ->
  unmock_rpc(),
  unmock_code_reload_enabled(),
  els_test_utils:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config)
     when TestCase =:= crossref orelse
           TestCase =:= crossref_pseudo_functions orelse
           TestCase =:= crossref_autoimport orelse
           TestCase =:= crossref_autoimport_disabled ->
  meck:unload(els_crossref_diagnostics),
  els_test_utils:end_per_testcase(TestCase, Config),
  els_mock_diagnostics:teardown(),
  ok;
end_per_testcase(TestCase, Config)
     when TestCase =:= code_path_extra_dirs orelse
          TestCase =:= use_long_names ->
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
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config),
  els_mock_diagnostics:teardown(),
  ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec bound_var_in_pattern(config()) -> ok.
bound_var_in_pattern(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_bound_var_in_pattern.erl"]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"BoundVarInPattern">>),
  els_test:assert_hints(
    [ #{ message => <<"Bound variable in pattern: Var1">>
       , range => {{5, 2}, {5, 6}}}
    , #{ message => <<"Bound variable in pattern: Var2">>
       , range => {{9, 9}, {9, 13}}}
    , #{ message => <<"Bound variable in pattern: Var4">>
       , range => {{17, 8}, {17, 12}}}
    , #{ message => <<"Bound variable in pattern: Var3">>
       , range => {{15, 10}, {15, 14}}}
    , #{ message => <<"Bound variable in pattern: Var5">>
       , range => {{23, 6}, {23, 10}}}
      %% erl_syntax_lib:annotate_bindings does not handle named funs
      %% correctly
      %% , #{ message => <<"Bound variable in pattern: New">>
      %%    , range => {{28, 6}, {28, 9}}}
      %% , #{ message => <<"Bound variable in pattern: F">>
      %%    , range => {{29, 6}, {29, 9}}}
    ], Diagnostics),
  ok.

-spec compiler(config()) -> ok.
compiler(_Config) ->
  Path = filename:join(["code_navigation", "src", "diagnostics.erl"]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_errors(
    [ #{ code => <<"L0000">>
       , message =>  <<"Issue in included file (1): bad attribute">>
       , range => {{3, 0}, {3, 35}}}
    , #{ code => <<"L0000">>
       , message =>  <<"Issue in included file (3): bad attribute">>
       , range => {{3, 0}, {3, 35}}}
    , #{ code => <<"L1295">>
       , message => <<"type undefined_type() undefined">>
       , range => {{5, 0}, {6, 0}}}
    ], Diagnostics),
  els_test:assert_warnings(
    [ #{ code => <<"L1230">>
       , message => <<"function main/1 is unused">>
       , range => {{6, 0}, {7, 0}}}
    ], Diagnostics),
  ok.

-spec compiler_with_behaviour(config()) -> ok.
compiler_with_behaviour(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_behaviour_impl.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [ #{ code => <<"L1284">>
       , message =>
           <<"undefined callback function one/0 "
             "(behaviour 'diagnostics_behaviour')">>
       , range => {{2, 0}, {3, 0}}},
      #{ code => <<"L1284">>
       , message =>
           <<"undefined callback function two/0 "
             "(behaviour 'diagnostics_behaviour')">>
       , range => {{2, 0}, {3, 0}}}
    ], Diagnostics).

%% Testing #614
-spec compiler_with_broken_behaviour(config()) -> ok.
compiler_with_broken_behaviour(_Config) ->
  Path = filename:join(["code_navigation", "src", "code_navigation.erl"]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_contains(
    #{ code => <<"L0000">>
     , message => <<"Issue in included file (5): syntax error before: ">>
     , range => {{2, 0}, {2, 24}}}, Diagnostics).

-spec compiler_with_custom_macros(config()) -> ok.
compiler_with_custom_macros(Config) ->
  %% This test uses priv/code_navigation/erlang_ls.config to define
  %% some macros.
  Path = filename:join(["code_navigation", "src", "diagnostics_macros.erl"]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  case ?config(columns, Config) of
    true ->
      %% diagnostic_macro has a spec with no '.' at the end
      %% which causes the poi for the spec to becomes the
      %% entire spec + function. So this range here is 8
      %% lines long.
      els_test:assert_errors(
        [#{ code => <<"E1507">>
          , message => <<"undefined macro 'UNDEFINED'">>
          , range => {{2, 0}, {10, 6}}
          }
        ], Diagnostics);
    false ->
      els_test:assert_errors(
        [#{ code => <<"E1507">>
          , message => <<"undefined macro 'UNDEFINED'">>
          , range => {{8, 0}, {9, 0}}
          }
        ], Diagnostics)
  end.

-spec compiler_with_parse_transform(config()) -> ok.
compiler_with_parse_transform(_Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_parse_transform_usage.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [#{ code => <<"L1268">>
      , message => <<"variable 'Args' is unused">>
      , range => {{6, 0}, {7, 0}}}
    ], Diagnostics).

-spec compiler_with_parse_transform_list(config()) -> ok.
compiler_with_parse_transform_list(_Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_parse_transform_usage_list.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [#{ code => <<"L1268">>
      , message => <<"variable 'Args' is unused">>
      , range => {{6, 0}, {7, 0}}}
    ], Diagnostics).

-spec compiler_with_parse_transform_included(config()) -> ok.
compiler_with_parse_transform_included(_Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_parse_transform_usage_included.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [#{ code => <<"L1268">>
      , message => <<"variable 'Args' is unused">>
      , range => {{6, 0}, {7, 0}}}
    ], Diagnostics).

-spec compiler_with_parse_transform_broken(config()) -> ok.
compiler_with_parse_transform_broken(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_parse_transform_usage_broken.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings([], Diagnostics),
  els_test:assert_errors(
    [#{ code => <<"L0000">>
      , message => <<"Issue in included file (10): syntax error before: ">>
      , range => {{4, 27}, {4, 61}}
      }
    , #{ code => <<"C1008">>
       , message => <<"undefined parse transform "
                      "'diagnostics_parse_transform_broken'">>
       , range => {{0, 0}, {1, 0}}
       }
    ], Diagnostics).

-spec compiler_with_parse_transform_deps(config()) -> ok.
compiler_with_parse_transform_deps(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_parse_transform_deps_a.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [ #{ code => <<"L1230">>
       , message => <<"function unused/0 is unused">>
       , range => {{4, 0}, {5, 0}}}
    ], Diagnostics),
  els_test:assert_errors([], Diagnostics).

-spec compiler_telemetry(config()) -> ok.
compiler_telemetry(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [ #{ code => <<"L1230">>
       , message => <<"function main/1 is unused">>
       , range => {{6, 0}, {7, 0}}}
    ], Diagnostics),
  els_test:assert_errors(
    [ #{ code => <<"L0000">>
       , message => <<"Issue in included file (1): bad attribute">>
       , range => {{3, 0}, {3, 35}}
       }
    , #{ code => <<"L0000">>
       , message => <<"Issue in included file (3): bad attribute">>
       , range => {{3, 0}, {3, 35}}
       }
    , #{ code => <<"L1295">>
       , message => <<"type undefined_type() undefined">>
       , range => {{5, 0}, {6, 0}}
       }], Diagnostics).

-spec code_path_extra_dirs(config()) -> ok.
code_path_extra_dirs(_Config) ->
  RootPath = binary_to_list(els_test_utils:root_path()),
  Dirs = [ AbsDir
           || Dir <- filelib:wildcard("*", RootPath),
           filelib:is_dir(AbsDir = filename:absname(Dir, RootPath))],
  ?assertMatch(true, lists:all(fun(Elem) -> code:del_path(Elem) end, Dirs)),
  ok.

-spec use_long_names(config()) -> ok.
use_long_names(_Config) ->
  {ok, HostName} = inet:gethostname(),
  NodeName = "my_node@" ++
             HostName ++ "." ++
             proplists:get_value(domain, inet:get_rc(), ""),
  Node = list_to_atom(NodeName),
  ?assertMatch(Node, els_config_runtime:get_node_name()),
  ok.

-spec epp_with_nonexistent_macro(config()) -> ok.
epp_with_nonexistent_macro(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "include"
                       , "nonexistent_macro.hrl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings([], Diagnostics),
  els_test:assert_errors(
    [ #{ code => <<"E1516">>
       , message => <<"can't find include file \"nonexisten-file.hrl\"">>
       , range => {{2, 0}, {3, 0}}
       }
    , #{ code => <<"E1507">>
       , message => <<"undefined macro 'MODULE'">>
       , range => {{4, 0}, {5, 0}}
       }
    , #{ code => <<"E1522">>
       , message => <<"-error(\"including nonexistent_macro.hrl "
                      "is not allowed\").">>
       , range => {{6, 0}, {7, 0}}}
    ], Diagnostics).

-spec elvis(config()) -> ok.
elvis(_Config) ->
  {ok, Cwd} = file:get_cwd(),
  RootPath = els_test_utils:root_path(),
  try
      file:set_cwd(RootPath),
      Path = filename:join([ "code_navigation"
                           , "src"
                           , "elvis_diagnostics.erl"
                           ]),
      {ok, Session} = els_test:start_session(Path),
      Diagnostics = els_test:wait_for_diagnostics(Session, <<"Elvis">>),
      els_test:assert_errors([], Diagnostics),
      els_test:assert_warnings(
        [ #{ code => operator_spaces
           , message => <<"Missing space right \",\" on line 6">>
           , range => {{5, 0}, {6, 0}}
           , relatedInformation => []
           }
        , #{ code => operator_spaces
           , message => <<"Missing space right \",\" on line 7">>
           , range => {{6, 0}, {7, 0}}
           , relatedInformation => []
           }
        ], Diagnostics)
  catch _Err ->
      file:set_cwd(Cwd)
  end,
  ok.

-spec escript(config()) -> ok.
escript(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics.escript"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings([], Diagnostics),
  els_test:assert_errors([], Diagnostics).

-spec escript_warnings(config()) -> ok.
escript_warnings(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_warnings.escript"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_warnings(
    [ #{ code => <<"L1230">>
       , message => <<"function unused/0 is unused">>
       , range => {{23, 0}, {24, 0}}
       }
    ], Diagnostics),
  els_test:assert_errors([], Diagnostics).

-spec escript_errors(config()) -> ok.
escript_errors(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_errors.escript"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"Compiler">>),
  els_test:assert_errors(
    [ #{ code => <<"P1711">>
       , message => <<"syntax error before: tion_with_error">>
       , range => {{23, 0}, {24, 0}}
       }
    ], Diagnostics),
  els_test:assert_warnings([], Diagnostics).

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
  meck:expect( rpc
             , call
             , fun(PNode, code, is_sticky, [_]) when PNode =:= NodeName ->
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
  Expected =
    [ #{ message => <<"Cannot find definition for function non_existing/0">>
       , range => {{6, 2}, {6, 14}}
       }
    , #{ message => <<"Cannot find definition for function lists:map/3">>
       , range => {{5, 2}, {5, 11}}
       }
    ],
  do_crossref_test("diagnostics_xref.erl", Expected).

%% #641
-spec crossref_pseudo_functions(config()) -> ok.
crossref_pseudo_functions(_Config) ->
  Expected =
    [ #{ message =>
          <<"Cannot find definition for function unknown_module:nonexistent/0">>
       , range => {{34, 2}, {34, 28}}
       }
    , #{ message =>
          <<"Cannot find definition for function unknown_module:module_info/1">>
       , range => {{13, 2}, {13, 28}}
       }
    , #{ message =>
           <<"Cannot find definition for function "
             "unknown_module:module_info/0">>
       , range => {{12, 2}, {12, 28}}
       }
    ],
  do_crossref_test("diagnostics_xref_pseudo.erl", Expected).

%% #860
-spec crossref_autoimport(config()) -> ok.
crossref_autoimport(_Config) ->

  %% This testcase cannot be run from an Erlang source tree version,
  %% it needs a released version.

  Expected = [],
  do_crossref_test("diagnostics_autoimport.erl", Expected).

%% #860
-spec crossref_autoimport_disabled(config()) -> ok.
crossref_autoimport_disabled(_Config) ->

  %% This testcase cannot be run from an Erlang source tree version,
  %% it needs a released version.

  Expected = [],
  do_crossref_test("diagnostics_autoimport_disabled.erl", Expected).

-spec do_crossref_test(string(), [els_diagnostics:diagnostic()]) -> ok.
do_crossref_test(TestModule, Expected) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , TestModule
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"CrossRef">>),
  els_test:assert_errors(Expected, Diagnostics).

-spec unused_includes(config()) -> ok.
unused_includes(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_unused_includes.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"UnusedIncludes">>),
  Expected = [#{ message => <<"Unused file: et.hrl">>
               , range => {{3, 0}, {3, 34}}
               }],
  els_test:assert_warnings(Expected, Diagnostics).

-spec unused_includes_compiler_attribute(config()) -> ok.
unused_includes_compiler_attribute(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_unused_includes_compiler_attribute.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"UnusedIncludes">>),
  Expected = [#{ message => <<"Unused file: file.hrl">>
               , range => {{3, 0}, {3, 40}}
               }],
  els_test:assert_warnings(Expected, Diagnostics).

-spec exclude_unused_includes(config()) -> ok.
exclude_unused_includes(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_unused_includes.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"UnusedIncludes">>),
  Expected = [],
  els_test:assert_warnings(Expected, Diagnostics).

-spec unused_macros(config()) -> ok.
unused_macros(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_unused_macros.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics = els_test:wait_for_diagnostics(Session, <<"UnusedMacros">>),
  Expected =
    [ #{ message => <<"Unused macro: UNUSED_MACRO">>
       , range => {{5, 8}, {5, 20}}
       },
      #{ message => <<"Unused macro: UNUSED_MACRO_WITH_ARG/1">>
       , range => {{6, 8}, {6, 29}}
       }
    ],
  els_test:assert_warnings(Expected, Diagnostics).

-spec unused_record_fields(config()) -> ok.
unused_record_fields(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_unused_record_fields.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics =
    els_test:wait_for_diagnostics(Session, <<"UnusedRecordFields">>),
  Expected =
    [ #{ message => <<"Unused record field: #unused_field.field_d">>
       , range => {{5, 32}, {5, 39}}
       }
    ],
  els_test:assert_warnings(Expected, Diagnostics).

-spec gradualizer(config()) -> ok.
gradualizer(_Config) ->
  Path = filename:join([ "code_navigation"
                       , "src"
                       , "diagnostics_gradualizer.erl"
                       ]),
  {ok, Session} = els_test:start_session(Path),
  Diagnostics =
    els_test:wait_for_diagnostics(Session, <<"Gradualizer">>),
  Expected =
    [ #{ message =>
           <<"The variable N is expected to have type integer() "
             "but it has type false | true\n">>
       , range => {{10, 0}, {11, 0}}}
    ],
  els_test:assert_warnings(Expected, Diagnostics).

%%==============================================================================
%% Internal Functions
%%==============================================================================

mock_rpc() ->
  meck:new(rpc, [passthrough, no_link, unstick]),
  {ok, HostName} = inet:gethostname(),
  NodeName = list_to_atom("fakenode@" ++ HostName),
  meck:expect( rpc
             , call
             , fun(PNode, c, c, [Module]) when PNode =:= NodeName ->
                   {ok, Module};
                  (Node, Mod, Fun, Args) ->
                   meck:passthrough([Node, Mod, Fun, Args])
               end
             ).

unmock_rpc() ->
  meck:unload(rpc).

mock_code_reload_enabled() ->
  meck:new(els_config, [passthrough, no_link]),
  meck:expect( els_config
             , get
             , fun(code_reload) ->
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
  meck:expect( els_config
             , get
             , fun(compiler_telemetry_enabled) ->
                   true;
                  (Key) ->
                   meck:passthrough([Key])
               end
             ),
  Self = self(),
  meck:expect( els_server
             , send_notification
             , fun(<<"telemetry/event">> = Method, Params) ->
                   Self ! {on_complete_telemetry, Params},
                   meck:passthrough([Method, Params]);
                  (M, P) ->
                   meck:passthrough([M, P])
               end
             ),
  ok.

%% -spec wait_for_compiler_telemetry() -> {uri(), [els_diagnostics:diagnostic()]}.
%% wait_for_compiler_telemetry() ->
%%   receive
%%     {on_complete_telemetry, Params} ->
%%       Params
%%   end.

unmock_compiler_telemetry_enabled() ->
  meck:unload(els_config),
  meck:unload(els_server).
