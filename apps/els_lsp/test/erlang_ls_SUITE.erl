-module(erlang_ls_SUITE).

-include("els_lsp.hrl").

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
-export([ parse_args/1
        , log_root/1
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

-spec groups() -> [atom()].
groups() ->
  els_test_utils:groups(?MODULE).

-spec init_per_suite(config()) -> config().
init_per_suite(_Config) ->
  [].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, _Config) ->
  [].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  unset_all_env(els_core),
  ok.

%%==============================================================================
%% Helpers
%%==============================================================================
-spec unset_all_env(atom()) -> ok.
unset_all_env(Application) ->
  Envs = application:get_all_env(Application),
  unset_env(Application, Envs).

-spec unset_env(atom(), list({atom(), term()})) -> ok.
unset_env(_Application, []) -> ok;
unset_env(Application, [{Par, _Val} | Rest]) ->
  application:unset_env(Application, Par),
  unset_env(Application, Rest).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec parse_args(config()) -> ok.
parse_args(_Config) ->
  Args = [ "--transport", "tcp"
         , "--port", "9000"
         , "--log-dir", "/test"
         , "--log-level", "error"
         ],
  erlang_ls:parse_args(Args),
  ?assertEqual(els_tcp, application:get_env(els_core, transport, undefined)),
  ?assertEqual(9000, application:get_env(els_core, port, undefined)),
  ?assertEqual('error', application:get_env(els_core, log_level, undefined)),
  ok.

-spec log_root(config()) -> ok.
log_root(_Config) ->
  meck:new(file, [unstick]),
  meck:expect(file, get_cwd, fun() -> {ok, "/root/els_lsp"} end),

  Args = [ "--transport", "tcp"
         , "--port", "9000"
         , "--log-dir", "/somewhere_else/logs"
         ],
  erlang_ls:parse_args(Args),
  ?assertEqual("/somewhere_else/logs/els_lsp", erlang_ls:log_root()),

  meck:unload(file),
  ok.
