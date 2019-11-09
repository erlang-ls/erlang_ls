-module(els_test_utils).

-export([ all/1
        , all/2
        , end_per_suite/1
        , end_per_testcase/2
        , groups/1
        , init_per_suite/1
        , init_per_testcase/2
        , wait_for/2
        ]).

-type config() :: [{atom(), any()}].

-include_lib("common_test/include/ct.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).
-define(HOSTNAME, {127,0,0,1}).
-define(PORT    , 10000).

-spec groups(module()) -> [{atom(), [atom()]}].
groups(Module) ->
  [ {tcp,   [], all(Module)}
  , {stdio, [], all(Module)}
  ].

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  PrivDir                = code:priv_dir(erlang_ls),
  RootPath               = filename:join([ list_to_binary(PrivDir)
                                         , ?TEST_APP]),
  RootUri                = els_uri:uri(RootPath),
  Path                   = filename:join([ RootPath
                                         , <<"src">>
                                         , <<"code_navigation.erl">>]),
  ExtraPath              = filename:join([ RootPath
                                         , <<"src">>
                                         , <<"code_navigation_extra.erl">>]),
  BehaviourPath          = filename:join([ RootPath
                                         , <<"src">>
                                         , <<"behaviour_a.erl">>]),
  IncludePath            = filename:join([ RootPath
                                         , <<"include">>
                                         , <<"code_navigation.hrl">>]),
  DiagnosticsPath        = filename:join([ RootPath
                                         , <<"src">>
                                         , <<"diagnostics.erl">>]),
  DiagnosticsIncludePath = filename:join([ RootPath
                                         , <<"include">>
                                         , <<"diagnostics.hrl">>]),

  Uri                    = els_uri:uri(Path),
  ExtraUri               = els_uri:uri(ExtraPath),
  BehaviourUri           = els_uri:uri(BehaviourPath),
  IncludeUri             = els_uri:uri(IncludePath),
  DiagnosticsUri         = els_uri:uri(DiagnosticsPath),
  DiagnosticsIncludeUri  = els_uri:uri(DiagnosticsIncludePath),

  {ok, Text} = file:read_file(Path),

  application:load(erlang_ls),
  application:set_env(erlang_ls, index_otp, false),
  application:set_env(erlang_ls, index_deps, false),

  [ {root_uri, RootUri}
  , {code_navigation_uri, Uri}
  , {code_navigation_text, Text}
  , {code_navigation_extra_uri, ExtraUri}
  , {behaviour_uri, BehaviourUri}
  , {include_uri, IncludeUri}
  , {diagnostics_uri, DiagnosticsUri}
  , {diagnostics_include_uri, DiagnosticsIncludeUri}
  | Config
  ].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  {Transport, Args} =
    case get_group(Config) of
      stdio ->
        ClientIo = els_fake_stdio:start(),
        ServerIo = els_fake_stdio:start(),
        els_fake_stdio:connect(ClientIo, ServerIo),
        els_fake_stdio:connect(ServerIo, ClientIo),

        ok = application:set_env(erlang_ls, transport, els_stdio),
        ok = application:set_env(erlang_ls, io_device, ServerIo),
        {stdio, ClientIo};
      tcp ->
        ok = application:set_env(erlang_ls, transport, els_tcp),
        {tcp, {?HOSTNAME, ?PORT}}
    end,

  {ok, Started} = application:ensure_all_started(erlang_ls),
  {ok, _} = els_client:start_link(Transport, Args),

  RootUri    = ?config(root_uri, Config),
  Uri        = ?config(code_navigation_uri, Config),
  Text       = ?config(code_navigation_text, Config),

  els_client:initialize(RootUri, []),
  els_client:did_open(Uri, erlang, 1, Text),

  %% Ensure modules used in test suites are indexed
  els_indexer:find_and_index_file("behaviour_a", async),
  els_indexer:find_and_index_file("code_navigation_extra", async),
  els_indexer:find_and_index_file("code_navigation.hrl", async),
  els_indexer:find_and_index_file("diagnostics.hrl", async),

  [{started, Started} | Config].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec wait_for(any(), non_neg_integer()) -> ok.
wait_for(_Message, Timeout) when Timeout =< 0 ->
  timeout;
wait_for(Message, Timeout) ->
  receive Message -> ok
  after 10 -> wait_for(Message, Timeout - 10)
  end.

-spec get_group(config()) -> atom().
get_group(Config) ->
  GroupProperties = ?config(tc_group_properties, Config),
  proplists:get_value(name, GroupProperties).
