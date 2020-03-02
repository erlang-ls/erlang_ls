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
init_per_testcase(TestCase, Config) when TestCase =:= code_reload orelse
                                         TestCase =:= code_reload_sticky_mod ->
  mock_rpc(),
  mock_code_reload_enabled(),
  els_test_utils:init_per_testcase(TestCase, Config);
init_per_testcase(TestCase, Config) ->
  mock_notifications(),
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(TestCase, Config) when TestCase =:= code_reload orelse
                                        TestCase =:= code_reload_sticky_mod ->
  unmock_rpc(),
  unmock_code_reload_enabled(),
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
  {Method, Params} = wait_for_notification(),
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
  ExpectedWarningRanges = [ #{'end' => #{character => 0,line => 7},
                              start => #{character => 0,line => 6}}
                          ],
  ?assertEqual(ExpectedWarningRanges, WarningRanges),
  ErrorRanges = [ Range || #{range := Range} <- Errors],
  ExpectedErrorRanges = [ #{'end' => #{character => 35,line => 3},
                            start => #{character => 0,line => 3}},
                          #{'end' => #{character => 35,line => 3},
                            start => #{character => 0,line => 3}},
                          #{'end' => #{character => 0,line => 6},
                            start => #{character => 0,line => 5}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_behaviour(config()) -> ok.
compiler_with_behaviour(Config) ->
  Uri = ?config(diagnostics_beh_impl_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(),
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
  ExpectedErrorRanges = [ #{ 'end' => #{character => 0,line => 3}
                           , start => #{character => 0,line => 2}}
                        , #{ 'end' => #{character => 0,line => 3}
                           , start => #{character => 0,line => 2}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_custom_macros(config()) -> ok.
compiler_with_custom_macros(Config) ->
  Uri = ?config(diagnostics_macros_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(),
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
  ExpectedErrorRanges = [ #{ 'end' => #{character => 0,line => 9}
                           , start => #{character => 0,line => 8}}
                        ],
  ?assertEqual(ExpectedErrorRanges, ErrorRanges),
  ok.

-spec compiler_with_parse_transform(config()) -> ok.
compiler_with_parse_transform(Config) ->
  Uri = ?config(diagnostics_parse_transform_usage_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(),
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
  ExpectedWarningsRanges = [ #{ 'end' => #{character => 0,line => 7}
                              , start => #{character => 0,line => 6}}
                           ],
  ?assertEqual(ExpectedWarningsRanges, WarningRanges),
  ok.

-spec compiler_with_parse_transform_included(config()) -> ok.
compiler_with_parse_transform_included(Config) ->
  Uri = ?config(diagnostics_parse_transform_included_uri, Config),
  ok = els_client:did_save(Uri),
  {Method, Params} = wait_for_notification(),
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
  ExpectedWarningsRanges = [ #{ 'end' => #{character => 0,line => 7}
                              , start => #{character => 0,line => 6}}
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
      %% Only the compiler diagnostics are in this notification
      {CMethod, CParams} = wait_for_notification(),
      ?assertEqual( <<"textDocument/publishDiagnostics">>
                  , CMethod),
      ?assert(maps:is_key(diagnostics, CParams)),
      #{diagnostics := CDiagnostics} = CParams,
      ?assertEqual(0, length(CDiagnostics)),

      %% Dialyzer and Elvis diagnostics are in this notification
      {Method, Params} = wait_for_notification(),
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
      ?assertEqual( #{'end' => #{character => 0,line => 6},
                      start => #{character => 0,line => 5}}
                  , WarningRange1
                  ),
      ?assertEqual( #{'end' => #{character => 0,line => 7},
                      start => #{character => 0,line => 6}}
                  , WarningRange2
                  )
  catch _Err ->
      file:set_cwd(Cwd)
  end,
  ok.

-spec code_reload(config()) -> ok.
code_reload(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  Module = els_uri:module(Uri),
  ok = els_text_synchronization:maybe_compile_and_load(Uri, []),
  ?assert(meck:called(rpc, call, ['fakenode', c, c, [Module]])),
  ok.

-spec code_reload_sticky_mod(config()) -> ok.
code_reload_sticky_mod(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  Module = els_uri:module(Uri),
  meck:expect( rpc
             , call
             , fun('fakenode', code, is_sticky, [_]) ->
                   true;
                  (Node, Mod, Fun, Args) ->
                   meck:passthrough([Node, Mod, Fun, Args])
               end
             ),
  ok = els_text_synchronization:maybe_compile_and_load(Uri, []),
  ?assert(meck:called(rpc, call, ['fakenode', code, is_sticky, [Module]])),
  ?assertNot(meck:called(rpc, call, ['fakenode', c, c, [Module]])),
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

wait_for_notification() ->
  receive
    {notification_sent, Method, Params} ->
      {Method, Params}
  end.

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

mock_code_reload_enabled() ->
  meck:new(els_config, [passthrough, no_link]),
  meck:expect( els_config
             , get
             , fun(code_reload) ->
                   [#{"node" => "fakenode"}];
                  (Key) ->
                   meck:passthrough([Key])
               end
             ).

unmock_code_reload_enabled() ->
  meck:unload(els_config).
