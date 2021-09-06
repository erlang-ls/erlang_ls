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
        , exclude_unused_includes/1
        , unused_macros/1
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
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config),
  els_mock_diagnostics:teardown(),
  ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec bound_var_in_pattern(config()) -> ok.
bound_var_in_pattern(Config) ->
  Uri = ?config(diagnostics_bound_var_in_pattern_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  Expected =
    [ #{message => <<"Bound variable in pattern: Var1">>,
        range =>
          #{'end' => #{character => 6, line => 5},
            start => #{character => 2, line => 5}},
        severity => 4,
        source => <<"BoundVarInPattern">>},
      #{message => <<"Bound variable in pattern: Var2">>,
        range =>
          #{'end' => #{character => 13, line => 9},
            start => #{character => 9, line => 9}},
        severity => 4,
        source => <<"BoundVarInPattern">>},
      #{message => <<"Bound variable in pattern: Var3">>,
        range =>
          #{'end' => #{character => 14, line => 15},
            start => #{character => 10, line => 15}},
        severity => 4,
        source => <<"BoundVarInPattern">>},
      #{message => <<"Bound variable in pattern: Var4">>,
        range =>
          #{'end' => #{character => 12, line => 17},
            start => #{character => 8, line => 17}},
        severity => 4,
        source => <<"BoundVarInPattern">>},
      #{message => <<"Bound variable in pattern: Var5">>,
        range =>
          #{'end' => #{character => 10, line => 23},
            start => #{character => 6, line => 23}},
        severity => 4,
        source => <<"BoundVarInPattern">>}
      %% erl_syntax_lib:annotate_bindings does not handle named funs correctly
      %% #{message => <<"Bound variable in pattern: New">>,
      %%   range =>
      %%     #{'end' => #{character => 9, line => 28},
      %%       start => #{character => 6, line => 28}},
      %%   severity => 4,
      %%   source => <<"BoundVarInPattern">>},
      %% #{message => <<"Bound variable in pattern: F">>,
      %%   range =>
      %%     #{'end' => #{character => 7, line => 29},
      %%       start => #{character => 6, line => 29}},
      %%   severity => 4,
      %%   source => <<"BoundVarInPattern">>}
    ],
  F = fun(#{message := M1}, #{message := M2}) -> M1 =< M2 end,
  Hints = [D || #{severity := ?DIAGNOSTIC_HINT} = D <- Diagnostics],
  ?assertEqual(Expected, lists:sort(F, Hints)),
  ok.

-spec compiler(config()) -> ok.
compiler(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(5, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(2, length(Warnings)),
  ?assertEqual(3, length(Errors)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningRanges =
    [#{'end' => #{character => 35, line => 3}
      , start => #{character => 0, line => 3}}
    ] ++
    fixcolumns(
      [ #{'end' => #{character => 4, line => 6}
         , start => #{character => 0, line => 6}}], Config),

  ?assertEqual(ExpectedWarningRanges, sort_ranges(WarningRanges)),
  ErrorRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRanges =
        [ #{'end' => #{character => 35, line => 3}
           , start => #{character => 0,  line => 3}},
          #{'end' => #{character => 35, line => 3}
           , start => #{character => 0,  line => 3}}] ++
        fixcolumns(
          [ #{'end' => #{character => 44, line => 5}
             , start => #{character => 30, line => 5}}], Config),
  ?assertEqual(ExpectedErrorRanges, sort_ranges(ErrorRanges)),
  ok.

-spec compiler_with_behaviour(config()) -> ok.
compiler_with_behaviour(Config) ->
  Uri = ?config(diagnostics_behaviour_impl_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(2, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(2, length(Warnings)),
  ErrorRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedErrorRanges =
        fixcolumns(
          [#{ 'end' => #{character => 34, line => 2}
            , start => #{character => 0, line => 2}},
           #{ 'end' => #{character => 34, line => 2}
            , start => #{character => 0, line => 2}}],
          Config),
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

%% Testing #614
-spec compiler_with_broken_behaviour(config()) -> ok.
compiler_with_broken_behaviour(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(24, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(18, length(Warnings)),
  Errors = [D || #{severity := ?DIAGNOSTIC_ERROR} = D <- Diagnostics],
  ?assertEqual(6, length(Errors)),
  [BehaviourError | _ ] = Errors,
  ExpectedError =
        #{message =>
              <<"Issue in included file (5): syntax error before: ">>
         , range =>
              #{'end' => #{character => 24, line => 2}
               , start => #{character => 0, line => 2}}
         , severity => 1
         , source => <<"Compiler">>
         , code => <<"L0000">>},
  ?assertEqual(ExpectedError, BehaviourError),
  ok.

-spec compiler_with_custom_macros(config()) -> ok.
compiler_with_custom_macros(Config) ->
  %% This test uses priv/code_navigation/erlang_ls.config to define some macros.
  Uri = ?config(diagnostics_macros_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(1, length(Errors)),
  [ErrorRange] = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRange =
        case ?config(columns, Config) of
            true ->
                %% diagnostic_macro has a spec with no '.' at the end
                %% which causes the poi for the spec to becomes the
                %% entire spec + function. So this range here is 8
                %% lines long.
                #{ 'end' => #{character => 6, line => 10},
                    start => #{character => 0, line => 2}};
            false ->
                #{ 'end' => #{character => 0, line => 9},
                    start => #{character => 0, line => 8}}
        end,
  ?assertEqual(ExpectedErrorRange, ErrorRange),
  ok.

-spec compiler_with_parse_transform(config()) -> ok.
compiler_with_parse_transform(Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Uri = ?config(diagnostics_parse_transform_usage_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges = fixcolumns(
                             [ #{ 'end' => #{character => 9, line => 6}
                                , start => #{character => 5, line => 6}}
                             ], Config),
  ?assertEqual(ExpectedWarningsRanges, WarningRanges),
  ok.

-spec compiler_with_parse_transform_list(config()) -> ok.
compiler_with_parse_transform_list(Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Uri = ?config(diagnostics_parse_transform_usage_list_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges = fixcolumns(
                             [ #{ 'end' => #{character => 9, line => 6}
                                , start => #{character => 5, line => 6}}
                             ], Config),
  ?assertEqual(ExpectedWarningsRanges, WarningRanges),
  ok.

-spec compiler_with_parse_transform_included(config()) -> ok.
compiler_with_parse_transform_included(Config) ->
  _ = code:delete(diagnostics_parse_transform),
  _ = code:purge(diagnostics_parse_transform),
  Uri = ?config(diagnostics_parse_transform_usage_included_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(2, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(2, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges =
        [#{ 'end' => #{character => 32, line => 4}
          , start => #{character => 0, line => 4}}
        ] ++
        fixcolumns(
          [ #{ 'end' => #{character => 9, line => 6}
             , start => #{character => 5, line => 6}}], Config),
  ?assertEqual(ExpectedWarningsRanges, sort_ranges(WarningRanges)),
  ok.

-spec compiler_with_parse_transform_broken(config()) -> ok.
compiler_with_parse_transform_broken(Config) ->
  Uri = ?config(diagnostics_parse_transform_usage_broken_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(2, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(0, length(Warnings)),
  Errors = [D || #{severity := ?DIAGNOSTIC_ERROR} = D <- Diagnostics],
  ?assertEqual(2, length(Errors)),
  ErrorsRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorsRanges = [#{'end' => #{character => 0, line => 1},
                            start => #{character => 0, line => 0}},
                          #{'end' => #{character => 61, line => 4},
                            start => #{character => 27, line => 4}}
                          ],
  ?assertEqual(ExpectedErrorsRanges, sort_ranges(ErrorsRanges)),
  ok.

-spec compiler_with_parse_transform_deps(config()) -> ok.
compiler_with_parse_transform_deps(Config) ->
  Uri = ?config(diagnostics_parse_transform_deps_a_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  Errors = [D || #{severity := ?DIAGNOSTIC_ERROR} = D <- Diagnostics],
  ?assertEqual(0, length(Errors)),
  WarningsRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges = fixcolumns(
                             [#{'end' => #{character => 6, line => 4},
                                start => #{character => 0, line => 4}}],
                             Config),
  ?assertEqual(ExpectedWarningsRanges, WarningsRanges),
  ok.

-spec compiler_telemetry(config()) -> ok.
compiler_telemetry(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(5, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(2, length(Warnings)),
  ?assertEqual(3, length(Errors)),
  ?assertEqual([<<"L1230">>], [Code || #{ code := Code } <- Warnings ]),
  ?assertEqual([<<"L0000">>, <<"L0000">>, <<"L1295">>]
              , [Code || #{ code := Code } <- Errors ]),
  Telemetry = wait_for_compiler_telemetry(),
  #{ type := Type
   , uri := UriT
   , diagnostics := DiagnosticsCodes }  = Telemetry,
  ?assertEqual(<<"erlang-diagnostic-codes">>, Type),
  ?assertEqual(Uri, UriT),
  ?assertEqual([ <<"L1230">>, <<"L0000">>, <<"L0000">>, <<"L1295">>]
               , DiagnosticsCodes),
  ok.

-spec code_path_extra_dirs(config()) -> ok.
code_path_extra_dirs(Config) ->
  RootPath = binary_to_list(?config(root_path, Config)),
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
epp_with_nonexistent_macro(Config) ->
  RootPath = ?config(root_path, Config),
  Path = filename:join([RootPath, <<"include">>, <<"nonexistent_macro.hrl">>]),
  Uri = els_uri:uri(Path),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(3, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(0, length(Warnings)),
  Errors = [D || #{severity := ?DIAGNOSTIC_ERROR} = D <- Diagnostics],
  ?assertEqual(3, length(Errors)),
  ErrorsRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorsRanges = [#{'end' => #{character => 0, line => 3},
                            start => #{character => 0, line => 2}},
                          #{'end' => #{character => 0, line => 5},
                            start => #{character => 0, line => 4}},
                          #{'end' => #{character => 0, line => 7},
                            start => #{character => 0, line => 6}}],
  ?assertEqual(ExpectedErrorsRanges, sort_ranges(ErrorsRanges)),
  ok.

-spec elvis(config()) -> ok.
elvis(Config) ->
  {ok, Cwd} = file:get_cwd(),
  RootPath = ?config(root_path, Config),
  try
      file:set_cwd(RootPath),
      Uri = ?config(elvis_diagnostics_uri, Config),
      els_mock_diagnostics:subscribe(),
      ok = els_client:did_save(Uri),
      Diagnostics = els_mock_diagnostics:wait_until_complete(),
      CDiagnostics = [D || #{source := <<"Compiler">>} = D <- Diagnostics],
      EDiagnostics = [D || #{source := <<"Elvis">>} = D <- Diagnostics],
      ?assertEqual(0, length(CDiagnostics)),
      ?assertEqual(2, length(EDiagnostics)),
      Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- EDiagnostics],
      Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- EDiagnostics],
      ?assertEqual(2, length(Warnings)),
      ?assertEqual(0, length(Errors)),
      [ #{range := WarningRange1}
      , #{range := WarningRange2} ] = Warnings,
      ?assertEqual( #{'end' => #{character => 0, line => 6},
                      start => #{character => 0, line => 5}}
                  , WarningRange1
                  ),
      ?assertEqual( #{'end' => #{character => 0, line => 7},
                      start => #{character => 0, line => 6}}
                  , WarningRange2
                  )
  catch _Err ->
      file:set_cwd(Cwd)
  end,
  ok.

-spec escript(config()) -> ok.
escript(Config) ->
  Uri = ?config(diagnostics_escript_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual([], Diagnostics),
  ok.

-spec escript_warnings(config()) -> ok.
escript_warnings(Config) ->
  Uri = ?config(diagnostics_warnings_escript_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual([], Errors),
  ?assertEqual(1, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningRanges = [ #{'end' => #{character => 0, line => 24},
                              start => #{character => 0, line => 23}}
                          ],
  ?assertEqual(ExpectedWarningRanges, WarningRanges),
  ok.

-spec escript_errors(config()) -> ok.
escript_errors(Config) ->
  Uri = ?config(diagnostics_errors_escript_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual([], Warnings),
  ?assertEqual(1, length(Errors)),
  ErrorRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRanges = [ #{'end' => #{character => 0, line => 24},
                            start => #{character => 0, line => 23}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

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
crossref(Config) ->
  Expected = [ #{ message =>
                    <<"Cannot find definition for function lists:map/3">>
                , range =>
                    #{ 'end' => #{character => 11, line => 5}
                     , start => #{character => 2, line => 5}}
                , severity => 1, source => <<"CrossRef">>}
             , #{ message =>
                    <<"Cannot find definition for function non_existing/0">>
                , range =>
                    #{ 'end' => #{character => 14, line => 6}
                     , start => #{character => 2, line => 6}}
                , severity => 1
                , source => <<"CrossRef">>
                }] ++
             fixcolumns(
               [#{ message =>
                       <<"function non_existing/0 undefined">>
                 , range =>
                       #{ 'end' => #{character => 14, line => 6}
                        , start => #{character => 2, line => 6}}
                 , severity => 1
                 , source => <<"Compiler">>
                 , code => <<"L1227">>
                 }
               ], Config),
  do_crossref_test(Config, diagnostics_xref_uri, Expected).


-spec do_crossref_test(config(), atom(), [map()]) -> ok.
do_crossref_test(Config, TestModule, ExpectedDiagnostics) ->
  Uri = ?config(TestModule, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  F = fun(#{message := M1}, #{message := M2}) -> M1 =< M2 end,
  ?assertEqual(ExpectedDiagnostics, lists:sort(F, Diagnostics)),
  ok.

%% #641
-spec crossref_pseudo_functions(config()) -> ok.
crossref_pseudo_functions(Config) ->
  Expected =
    [#{message =>
         <<"Cannot find definition for function unknown_module:module_info/0">>,
       range =>
         #{'end' => #{character => 28, line => 12},
           start => #{character => 2, line => 12}},
       severity => 1, source => <<"CrossRef">>},
     #{message =>
         <<"Cannot find definition for function unknown_module:module_info/1">>,
       range =>
         #{'end' => #{character => 28, line => 13},
           start => #{character => 2, line => 13}},
       severity => 1, source => <<"CrossRef">>},
     #{message =>
         <<"Cannot find definition for function unknown_module:nonexistent/0">>,
       range =>
         #{'end' => #{character => 28, line => 34},
           start => #{character => 2, line => 34}},
       severity => 1, source => <<"CrossRef">>}],
  do_crossref_test(Config, diagnostics_xref_pseudo_uri, Expected).

%% #860
-spec crossref_autoimport(config()) -> ok.
crossref_autoimport(Config) ->

  %% This testcase cannot be run from an Erlang source tree version,
  %% it needs a released version.

  Expected = [],
  do_crossref_test(Config, diagnostics_autoimport_uri, Expected).

%% #860
-spec crossref_autoimport_disabled(config()) -> ok.
crossref_autoimport_disabled(Config) ->

  %% This testcase cannot be run from an Erlang source tree version,
  %% it needs a released version.

  Expected =
    fixcolumns(
      [#{message =>
         <<"function atom_to_list/1 undefined">>,
       range =>
         #{'end' => #{character => 22, line => 6},
           start => #{character => 4, line => 6}},
       severity => 1, source => <<"Compiler">>, code => <<"L1227">>}], Config),
  do_crossref_test(Config, diagnostics_autoimport_disabled_uri, Expected).

-spec unused_includes(config()) -> ok.
unused_includes(Config) ->
  Uri = ?config(diagnostics_unused_includes_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  Expected = [ #{ message => <<"Unused file: et.hrl">>
                , range =>
                    #{ 'end' => #{ character => 34
                                 , line => 3
                                 }
                     , start => #{ character => 0
                                 , line => 3
                                 }
                     }
                , severity => 2
                , source => <<"UnusedIncludes">>
                }
             ],
  F = fun(#{message := M1}, #{message := M2}) -> M1 =< M2 end,
  ?assertEqual(Expected, lists:sort(F, Diagnostics)),
  ok.

-spec exclude_unused_includes(config()) -> ok.
exclude_unused_includes(Config) ->
  Uri = ?config(diagnostics_unused_includes_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  ?assertEqual([], Diagnostics),
  ok.

-spec unused_macros(config()) -> ok.
unused_macros(Config) ->
  Uri = ?config(diagnostics_unused_macros_uri, Config),
  els_mock_diagnostics:subscribe(),
  ok = els_client:did_save(Uri),
  Diagnostics = els_mock_diagnostics:wait_until_complete(),
  Expected = [ #{ message => <<"Unused macro: UNUSED_MACRO">>
                , range =>
                    #{ 'end' => #{ character => 20
                                 , line => 5
                                 }
                     , start => #{ character => 8
                                 , line => 5
                                 }
                     }
                , severity => 2
                , source => <<"UnusedMacros">>
                },
                #{ message => <<"Unused macro: UNUSED_MACRO_WITH_ARG/1">>
                , range =>
                    #{ 'end' => #{ character => 29
                                 , line => 6
                                 }
                     , start => #{ character => 8
                                 , line => 6
                                 }
                     }
                , severity => 2
                , source => <<"UnusedMacros">>
                }
             ],
  F = fun(#{message := M1}, #{message := M2}) -> M1 =< M2 end,
  ?assertEqual(Expected, lists:sort(F, Diagnostics)),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec fixcolumns([range()], config()) -> [range()].
fixcolumns(Ranges, Config) ->
    case ?config(columns, Config) of
        true ->
            Ranges;
        _ ->
            lists:map(
              fun F(#{ start := #{ character := StartCol
                                 , line := StartLine }
                     , 'end' := #{ character := EndCol
                                 , line := EndLine }
                     } = Range)
                    when EndCol =/= 0; StartCol =/= 0 ->
                      Range#{ 'end' => #{ character => 0
                                        , line => EndLine + 1 },
                              start => #{ character => 0
                                        , line => StartLine } };
                  F(#{ range := Range } = Message) ->
                      Message#{ range := F(Range) };
                  F(Range) ->
                      Range
              end, Ranges)
    end.

-spec sort_ranges([range()]) -> [range()].
sort_ranges(Ranges) ->
  lists:sort(fun(#{start := #{line := L1, character := C1}},
                 #{start := #{line := L2, character := C2}}) ->
                 {L1, C1} =< {L2, C2}
             end,
             Ranges).

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

-spec wait_for_compiler_telemetry() -> {uri(), [els_diagnostics:diagnostic()]}.
wait_for_compiler_telemetry() ->
  receive
    {on_complete_telemetry, Params} ->
      Params
  end.

unmock_compiler_telemetry_enabled() ->
  meck:unload(els_config),
  meck:unload(els_server).
