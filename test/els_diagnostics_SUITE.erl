-module(els_diagnostics_SUITE).

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
-export([ compiler/1
        , compiler_with_behaviour/1
        , compiler_with_custom_macros/1
        , compiler_with_parse_transform/1
        , compiler_with_parse_transform_included/1
        , code_reload/1
        , code_reload_sticky_mod/1
        , dialyzer/1
        , elvis/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include("erlang_ls.hrl").

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

-spec all() -> [{group, stdio | tcp}].
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
init_per_testcase(TestCase, Config0) when TestCase =:= code_reload orelse
                                          TestCase =:= code_reload_sticky_mod ->
  mock_notifications(),
  Config = els_test_utils:init_per_testcase(TestCase, Config0),
  code_reload_init_per_testcase(),
  Config;
init_per_testcase(TestCase, Config0) when TestCase =:= dialyzer ->
  mock_notifications(),
  Config = els_test_utils:init_per_testcase(TestCase, Config0),
  dialyzer_init_per_testcase(),
  Config;
init_per_testcase(TestCase, Config) ->
  mock_notifications(),
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) when TestCase =:= code_reload orelse
                                        TestCase =:= code_reload_sticky_mod ->
  unmock_notifications(),
  code_reload_end_per_testcase(),
  els_test_utils:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config) when TestCase =:= dialyzer ->
  unmock_notifications(),
  dialyzer_end_per_testcase(),
  els_test_utils:end_per_testcase(TestCase, Config);
end_per_testcase(TestCase, Config) ->
  els_test_utils:end_per_testcase(TestCase, Config),
  unmock_notifications(),
  ok.

%%==============================================================================
%% Testcases
%%==============================================================================
-spec compiler(config()) -> ok.
compiler(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_compiler_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(4, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  ?assertEqual(3, length(Errors)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningRanges = [ #{'end' => #{character => 0, line => 7},
                              start => #{character => 0, line => 6}}
                          ],
  ?assertEqual(ExpectedWarningRanges, WarningRanges),
  ErrorRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRanges = [ #{'end' => #{character => 35, line => 3},
                            start => #{character => 0, line => 3}},
                          #{'end' => #{character => 35, line => 3},
                            start => #{character => 0, line => 3}},
                          #{'end' => #{character => 0, line => 6},
                            start => #{character => 0, line => 5}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_behaviour(config()) -> ok.
compiler_with_behaviour(Config) ->
  Uri = ?config(diagnostics_beh_impl_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_compiler_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(2, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(2, length(Warnings)),
  ErrorRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedErrorRanges = [ #{ 'end' => #{character => 0, line => 3}
                           , start => #{character => 0, line => 2}}
                        , #{ 'end' => #{character => 0, line => 3}
                           , start => #{character => 0, line => 2}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_custom_macros(config()) -> ok.
compiler_with_custom_macros(Config) ->
  Uri = ?config(diagnostics_macros_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_compiler_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(1, length(Diagnostics)),
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(1, length(Errors)),
  ErrorRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRanges = [ #{ 'end' => #{character => 0, line => 9}
                           , start => #{character => 0, line => 8}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_parse_transform(config()) -> ok.
compiler_with_parse_transform(Config) ->
  Uri = ?config(diagnostics_parse_transform_usage_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_compiler_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges = [ #{ 'end' => #{character => 0, line => 7}
                              , start => #{character => 0, line => 6}}
                           ],
  ?assertEqual(ExpectedWarningsRanges, WarningRanges),
  ok.

-spec compiler_with_parse_transform_included(config()) -> ok.
compiler_with_parse_transform_included(Config) ->
  Uri = ?config(diagnostics_parse_transform_included_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_compiler_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningsRanges = [ #{ 'end' => #{character => 0, line => 7}
                              , start => #{character => 0, line => 6}}
                           ],
  ?assertEqual(ExpectedWarningsRanges, WarningRanges),
  ok.

-spec elvis(config()) -> ok.
elvis(Config) ->
  {ok, Cwd} = file:get_cwd(),
  RootPath = ?config(root_path, Config),
  try
      file:set_cwd(RootPath),
      Uri = ?config(elvis_diagnostics_uri, Config),
      ok = els_client:did_save(Uri),
      {Method, Params} = wait_for_notification(els_elvis_diagnostics),
      ?assertEqual( <<"textDocument/publishDiagnostics">>
                  , Method),
      ?assert(maps:is_key(uri, Params)),
      #{uri := Uri} = Params,
      ?assert(maps:is_key(diagnostics, Params)),
      #{diagnostics := Diagnostics} = Params,
      ?assertEqual(2, length(Diagnostics)),
      Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
      Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
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

-spec code_reload(config()) -> ok.
code_reload(Config) ->
  Uri = ?config(diagnostics_no_errors_uri, Config),
  Module = els_uri:module(Uri),
  ok = els_client:did_save(Uri),
  {_Method, _Params} = wait_for_notification(any),
  ?assert(meck:called(rpc, call, ['fakenode', c, c, [Module]])),
  ok.

-spec code_reload_sticky_mod(config()) -> ok.
code_reload_sticky_mod(Config) ->
  Uri = ?config(diagnostics_no_errors_uri, Config),
  Module = els_uri:module(Uri),
  meck:expect( rpc
             , call
             , fun('fakenode', code, is_sticky, [Mod]) when Mod =:= Module ->
                   true;
                  (Node, Mod, Fun, Args) ->
                   meck:passthrough([Node, Mod, Fun, Args])
               end
             ),
  ok = els_client:did_save(Uri),
  {_Method, _Params} = wait_for_notification(any),
  ?assert(meck:called(rpc, call, ['fakenode', code, is_sticky, [Module]])),
  ?assertNot(meck:called(rpc, call, ['fakenode', c, c, [Module]])),
  ok.

-spec dialyzer(config()) -> ok.
dialyzer(Config) ->
  Uri = ?config(diagnostics_dialyzer_uri, Config),
  Plt = filename:join([ code:priv_dir(erlang_ls)
                      , <<"code_navigation">>
                      , <<"dialyzer.plt">>]),
  meck:expect(
    els_dialyzer_diagnostics, run_dialyzer, 1,
    fun(_) ->
        Args = [ {files, [binary_to_list(els_uri:path(Uri))]}
               , {from, src_code}
               , {plts, [binary_to_list(Plt)]}
               ],
        meck:passthrough([Args])
    end),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(els_dialyzer_diagnostics),
  ?assertEqual( <<"textDocument/publishDiagnostics">>
              , Method),
  ?assert(maps:is_key(uri, Params)),
  #{uri := Uri} = Params,
  ?assert(maps:is_key(diagnostics, Params)),
  #{diagnostics := Diagnostics} = Params,
  ?assertEqual(1, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  ?assertEqual(0, length(Errors)),
  WarningRanges = [ Range || #{range := Range} <- Warnings],
  ExpectedWarningRanges = [ #{'end' => #{character => -1,line => 5},
                              start => #{character => -1,line => 5}}
                          ],
  ?assertEqual(ExpectedWarningRanges, WarningRanges),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
mock_notifications() ->
  meck:new(els_server, [passthrough, no_link]),
  Self = self(),
  meck:expect( els_server
             , send_notification
             , fun(Method, Params) ->
                   Self ! {notification_sent, Method, Params},
                   ok
               end
             ).

unmock_notifications() ->
  meck:unload(els_server).

wait_for_notification(Type) ->
  receive
    {notification_sent, Method, Params} ->
      case Type of
        any -> {Method, Params};
        _ ->
          case filter_for_type(Type, Params) of
            retry -> wait_for_notification(Type);
            {ok, _} ->
              {Method, Params}
          end
      end
  end.

filter_for_type(_Type, #{message := <<"code_reload", _/binary>>}) -> retry;
filter_for_type(Type, Params) ->
  Source = apply(Type, source, []),
  Diagnostics = maps:get(diagnostics, Params, []),
  Res = lists:filter(fun(Diagnostic) ->
                         maps:get(source, Diagnostic) =:= Source
                     end, Diagnostics),
  case Res of
    [] -> retry;
    Res -> {ok, Params}
  end.


code_reload_init_per_testcase() ->
  mock_rpc(),
  els_config:set(code_reload, #{"node" => "fakenode"}).

code_reload_end_per_testcase() ->
  unmock_rpc(),
  els_config:set(code_reload, disabled).

mock_rpc() ->
  meck:new(rpc, [passthrough, no_link, unstick]),
  meck:expect( rpc
             , call
             , fun('fakenode', c, c, [Module]) ->
                   {ok, Module};
                  (Node, Mod, Fun, Args) ->
                   meck:passthrough([Node, Mod, Fun, Args])
               end
             ).

unmock_rpc() ->
  meck:unload(rpc).

dialyzer_init_per_testcase() ->
  mock_dialyzer(),
  els_config:set(plt_path, [mocked]).

dialyzer_end_per_testcase() ->
  unmock_dialyzer(),
  els_config:set(plt_path, undefined).

mock_dialyzer() ->
  meck:new(els_dialyzer_diagnostics, [passthrough, no_link]).

unmock_dialyzer() ->
  meck:unload([els_dialyzer_diagnostics]).
