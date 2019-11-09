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

-spec all() -> [atom()].
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
  mock_notifications(),
  els_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
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
  ?assertEqual(2, length(Diagnostics)),
  Warnings = [D || #{severity := ?DIAGNOSTIC_WARNING} = D <- Diagnostics],
  Errors   = [D || #{severity := ?DIAGNOSTIC_ERROR}   = D <- Diagnostics],
  ?assertEqual(1, length(Warnings)),
  ?assertEqual(1, length(Errors)),
  [#{range := WarningRange}] = Warnings,
  [#{range := ErrorRange}] = Errors,
  ?assertEqual( #{'end' => #{character => 0,line => 5},
                  start => #{character => 0,line => 5}}
              , WarningRange
              ),
  ?assertEqual( #{'end' => #{character => 0,line => 4},
                  start => #{character => 0,line => 4}}
              , ErrorRange
              ),
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
mock_notifications() ->
  meck:new(els_server, [passthrough, no_link]),
  Self = self(),
  meck:expect( els_server
             , send_notification
             , fun(_Server, Method, Params) ->
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
