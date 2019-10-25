-module(erlang_ls_test_utils).

-export([ all/1
        , all/2
        , init_per_testcase/2
        , wait_for/2
        ]).

-type config() :: [{atom(), any()}].

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).
-define(HOSTNAME, {127,0,0,1}).
-define(PORT    , 10000).

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  {ok, _}       = erlang_ls_client:start_link(?HOSTNAME, ?PORT),
  RootPath      = filename:join([ list_to_binary(code:priv_dir(erlang_ls))
                                , ?TEST_APP]),
  RootUri       = erlang_ls_uri:uri(RootPath),
  erlang_ls_client:initialize(RootUri, []),
  Path          = filename:join([ RootPath
                             , <<"src">>
                             , <<"code_navigation.erl">>]),
  ExtraPath     = filename:join([ RootPath
                             , <<"src">>
                             , <<"code_navigation_extra.erl">>]),
  BehaviourPath = filename:join([ RootPath
                                , <<"src">>
                                , <<"behaviour_a.erl">>]),
  IncludePath   = filename:join([ RootPath
                                , <<"include">>
                                , <<"code_navigation.hrl">>]),
  DiagnosticsPath = filename:join([ RootPath
                                  , <<"src">>
                                  , <<"diagnostics.erl">>]),
  DiagnosticsIncludePath = filename:join([ RootPath
                                         , <<"include">>
                                         , <<"diagnostics.hrl">>]),
  Uri                   = erlang_ls_uri:uri(Path),
  ExtraUri              = erlang_ls_uri:uri(ExtraPath),
  BehaviourUri          = erlang_ls_uri:uri(BehaviourPath),
  IncludeUri            = erlang_ls_uri:uri(IncludePath),
  DiagnosticsUri        = erlang_ls_uri:uri(DiagnosticsPath),
  DiagnosticsIncludeUri = erlang_ls_uri:uri(DiagnosticsIncludePath),
  {ok, Text}    = file:read_file(Path),
  erlang_ls_client:did_open(Uri, erlang, 1, Text),
  [ {code_navigation_uri, Uri}
  , {code_navigation_extra_uri, ExtraUri}
  , {behaviour_uri, BehaviourUri}
  , {include_uri, IncludeUri}
  , {diagnostics_uri, DiagnosticsUri}
  , {diagnostics_include_uri, DiagnosticsIncludeUri}
    |Config].

-spec wait_for(any(), non_neg_integer()) -> ok.
wait_for(_Message, Timeout) when Timeout =< 0 ->
  timeout;
wait_for(Message, Timeout) ->
  receive Message -> ok
  after 10 -> wait_for(Message, Timeout - 10)
  end.
