-module(erlang_ls_SUITE).

-include("erlang_ls.hrl").

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
        , lager_config/1
        , log_handlers/1
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
  unset_all_env(erlang_ls),
  unset_all_env(lager),
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
         , "--log-level", "=error"
         ],
  erlang_ls:parse_args(Args),
  ?assertEqual(els_tcp, application:get_env(erlang_ls, transport, undefined)),
  ?assertEqual(9000, application:get_env(erlang_ls, port, undefined)),
  ?assertEqual("/test", application:get_env(erlang_ls, log_dir, undefined)),
  ?assertEqual("=error", application:get_env(erlang_ls, log_level, undefined)),
  ok.

-spec lager_config(config()) -> ok.
lager_config(_Config) ->
  meck:new(filelib, [unstick]),
  meck:expect(filelib, ensure_dir, fun(_) -> ok end),

  application:set_env(erlang_ls, logging_enabled, false),
  erlang_ls:lager_config(),
  ?assertEqual(0, length(application:get_env(lager, handlers, [failure]))),
  ?assertEqual(false, application:get_env(lager, crash_log, undefined)),

  application:set_env(erlang_ls, logging_enabled, true),
  erlang_ls:lager_config(),
  ?assertEqual(1, length(application:get_env(lager, handlers, []))),
  ?assertEqual("crash.log", application:get_env(lager, crash_log, undefined)),

  meck:unload(filelib),
  ok.

-spec log_handlers(config()) -> ok.
log_handlers(_Config) ->
  meck:new(filelib, [unstick]),
  meck:expect(filelib, ensure_dir, fun(_) -> ok end),

  Handlers = erlang_ls:lager_handlers("/some/directory"),
  ExpectedHandlers = [ { lager_file_backend
                       , [ {file, "/some/directory/server.log"}
                         , {level, "info"}
                         ]
                       }
                     ],
  ?assertEqual(ExpectedHandlers, Handlers),

  meck:unload(filelib),
  ok.

-spec log_root(config()) -> ok.
log_root(_Config) ->
  meck:new(file, [unstick]),
  meck:expect(file, get_cwd, fun() -> {ok, "/root/erlang_ls"} end),

  Args = [ "--transport", "tcp"
         , "--port", "9000"
         , "--log-dir", "/somewhere_else/logs"
         ],
  erlang_ls:parse_args(Args),
  ?assertEqual("/somewhere_else/logs/erlang_ls", erlang_ls:log_root()),

  meck:unload(file),
  ok.
