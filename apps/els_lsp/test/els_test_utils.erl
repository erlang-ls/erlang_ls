-module(els_test_utils).

-export([ all/1
        , all/2
        , end_per_suite/1
        , end_per_testcase/2
        , init_per_suite/1
        , init_per_testcase/2
        , start/0
        , wait_for/2
        , wait_for_fun/3
        , wait_until_mock_called/2
        , root_path/0
        , root_uri/0
        ]).

-include_lib("common_test/include/ct.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, "code_navigation").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% API
%%==============================================================================

-spec all(module()) -> [atom()].
all(Module) -> all(Module, []).

-spec all(module(), [atom()]) -> [atom()].
all(Module, Functions) ->
  ExcludedFuns = [init_per_suite, end_per_suite, all, module_info | Functions],
  Exports = Module:module_info(exports),
  [F || {F, 1} <- Exports, not lists:member(F, ExcludedFuns)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  application:load(els_core),
  Config.

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  meck:new(els_distribution_server, [no_link, passthrough]),
  meck:expect(els_distribution_server, connect, 0, ok),
  Started   = start(),
  els_client:initialize(root_uri(), #{indexingEnabled => false}),
  els_client:initialized(),
  SrcConfig = lists:flatten([index_file(S) || S <- sources()]),
  TestConfig = lists:flatten([index_file(S) || S <- tests()]),
  EscriptConfig = lists:flatten([index_file(S) || S <- escripts()]),
  IncludeConfig = lists:flatten([index_file(S) || S <- includes()]),
  lists:append( [ SrcConfig
                , TestConfig
                , EscriptConfig
                , IncludeConfig
                , [ {started, Started}
                  | Config]
                ]).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, Config) ->
  meck:unload(els_distribution_server),
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec start() -> [atom()].
start() ->
  ClientIo = els_fake_stdio:start(),
  ServerIo = els_fake_stdio:start(),
  els_fake_stdio:connect(ClientIo, ServerIo),
  els_fake_stdio:connect(ServerIo, ClientIo),
  ok = application:set_env(els_core, io_device, ServerIo),
  {ok, Started} = application:ensure_all_started(els_lsp),
  els_client:start_link(#{io_device => ClientIo}),
  Started.

-spec wait_for(any(), non_neg_integer()) -> ok.
wait_for(_Message, Timeout) when Timeout =< 0 ->
  timeout;
wait_for(Message, Timeout) ->
  receive Message -> ok
  after 10 -> wait_for(Message, Timeout - 10)
  end.

-spec wait_for_fun(fun(), non_neg_integer(), non_neg_integer())
                  -> {ok, any()} | timeout.
wait_for_fun(_CheckFun, _WaitTime, 0) ->
  timeout;
wait_for_fun(CheckFun, WaitTime, Retries) ->
  case CheckFun() of
    true ->
      ok;
    {true, Value} ->
      {ok, Value};
    false ->
      timer:sleep(WaitTime),
      wait_for_fun(CheckFun, WaitTime, Retries - 1)
  end.

-spec sources() -> [binary()].
sources() ->
  wildcard("*.erl", "src").

-spec tests() -> [binary()].
tests() ->
  wildcard("*.erl", "test").

-spec escripts() -> [binary()].
escripts() ->
  wildcard("*.escript", "src").

-spec includes() -> [binary()].
includes() ->
  wildcard("*.hrl", "include").

%% @doc Index a file and produce the respective config entries
%%
%%      Given the path to a source file, index it and produce a config
%%      containing the respective path, uri and text to simplify
%%      accessing this information from test cases.
-spec index_file(binary()) -> [{atom(), any()}].
index_file(Path) ->
  Uri = els_uri:uri(Path),
  els_indexing:ensure_deeply_indexed(Uri),
  {ok, Text} = file:read_file(Path),
  ConfigId = config_id(Path),
  [ {atoms_append(ConfigId, '_path'), Path}
  , {atoms_append(ConfigId, '_uri'), Uri}
  , {atoms_append(ConfigId, '_text'), Text}
  ].

-spec suffix(binary()) -> binary().
suffix(<<".erl">>) -> <<"">>;
suffix(<<".hrl">>) -> <<"_h">>;
suffix(<<".escript">>) -> <<"_escript">>.

-spec config_id(string()) -> atom().
config_id(Path) ->
  Extension = filename:extension(Path),
  BaseName = filename:basename(Path, Extension),
  Suffix = suffix(Extension),
  binary_to_atom(<<BaseName/binary, Suffix/binary>>, utf8).

-spec atoms_append(atom(), atom()) -> atom().
atoms_append(Atom1, Atom2) ->
  Bin1 = atom_to_binary(Atom1, utf8),
  Bin2 = atom_to_binary(Atom2, utf8),
  binary_to_atom(<<Bin1/binary, Bin2/binary>>, utf8).

-spec wait_until_mock_called(atom(), atom()) -> ok.
wait_until_mock_called(M, F) ->
  case meck:num_calls(M, F, '_') of
    0 ->
      timer:sleep(100),
      wait_until_mock_called(M, F);
    _ ->
      ok
  end.

-spec root_path() -> binary().
root_path() ->
  PrivDir = code:priv_dir(els_lsp),
  els_utils:to_binary(filename:join([PrivDir, ?TEST_APP])).

-spec root_uri() -> els_uri:uri().
root_uri() ->
  els_uri:uri(root_path()).

-spec wildcard(string(), string()) -> [binary()].
wildcard(Extension, Dir) ->
  RootDir = els_utils:to_list(root_path()),
  [els_utils:to_binary(filename:join([RootDir, Dir, Path])) ||
    Path <- filelib:wildcard(Extension, filename:join([RootDir, Dir]))].
