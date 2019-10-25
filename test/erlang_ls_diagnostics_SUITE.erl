-module(erlang_ls_diagnostics_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
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

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(erlang_ls),
  [{started, Started}|Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config0) ->
  Config = erlang_ls_test_utils:init_per_testcase(TestCase, Config0),
  mock_notifications(),
  Config.

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  unmock_notifications(),
  ok.

-spec all() -> [atom()].
all() ->
  erlang_ls_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec compiler(config()) -> ok.
compiler(Config) ->
  Uri = ?config(diagnostics_uri, Config),
  ok = erlang_ls_client:did_save(Uri),
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
  meck:new(erlang_ls_server, [passthrough, no_link]),
  Self = self(),
  meck:expect( erlang_ls_server
             , send_notification
             , fun(_Server, Method, Params) ->
                   Self ! {notification_sent, Method, Params},
                   ok
               end
             ).

unmock_notifications() ->
  meck:unload(erlang_ls_server).

wait_for_notification() ->
  receive
    {notification_sent, Method, Params} ->
      {Method, Params}
  end.
