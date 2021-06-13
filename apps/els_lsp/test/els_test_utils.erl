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
        ]).

-include_lib("common_test/include/ct.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(TEST_APP, <<"code_navigation">>).

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].
-type file_type() :: src | test | include | escript.

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
  PrivDir = code:priv_dir(els_lsp),
  RootPath = filename:join([ els_utils:to_binary(PrivDir)
                           , ?TEST_APP]),
  RootUri = els_uri:uri(RootPath),
  application:load(els_core),
  [ {root_uri, RootUri}
  , {root_path, RootPath}
  | Config ].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_TestCase, Config) ->
  meck:new(els_distribution_server, [no_link, passthrough]),
  meck:expect(els_distribution_server, connect, 0, ok),
  Started   = start(),
  RootPath  = ?config(root_path, Config),
  RootUri   = ?config(root_uri, Config),
  els_client:initialize(RootUri, #{indexingEnabled => false}),
  els_client:initialized(),
  SrcConfig = lists:flatten(
                [index_file(RootPath, src, S) || S <- sources()]),
  TestConfig = lists:flatten(
                 [index_file(RootPath, test, S) || S <- tests()]),
  EscriptConfig = lists:flatten(
                    [index_file(RootPath, escript, S) || S <- escripts()]),
  IncludeConfig = lists:flatten(
                    [index_file(RootPath, include, S) || S <- includes()]),
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

-spec sources() -> [atom()].
sources() ->
  [ 'diagnostics.new'
  , behaviour_a
  , code_lens_function_references
  , code_navigation
  , code_navigation_extra
  , code_navigation_types
  , code_navigation_undefined
  , 'Code.Navigation.Elixirish'
  , completion
  , completion_caller
  , completion_resolve
  , completion_resolve_2
  , completion_snippets
  , completion_attributes
  , completion_incomplete
  , diagnostics
  , diagnostics_bound_var_in_pattern
  , diagnostics_autoimport
  , diagnostics_autoimport_disabled
  , diagnostics_behaviour
  , diagnostics_behaviour_impl
  , diagnostics_macros
  , diagnostics_parse_transform
  , diagnostics_parse_transform_broken
  , diagnostics_parse_transform_deps_a
  , diagnostics_parse_transform_deps_b
  , diagnostics_parse_transform_deps_c
  , diagnostics_parse_transform_usage
  , diagnostics_parse_transform_usage_list
  , diagnostics_parse_transform_usage_broken
  , diagnostics_parse_transform_usage_included
  , diagnostics_xref
  , diagnostics_xref_pseudo
  , diagnostics_unused_includes
  , diagnostics_unused_macros
  , elvis_diagnostics
  , execute_command_suggest_spec
  , format_input
  , hover_docs
  , hover_docs_caller
  , hover_macro
  , hover_record_expr
  , implementation
  , implementation_a
  , implementation_b
  , my_gen_server
  , rename
  , rename_function
  , rename_function_import
  , rename_type
  , rename_usage1
  , rename_usage2
  , rename_variable
  ].

tests() ->
  [ sample_SUITE
  ].

-spec escripts() -> [atom()].
escripts() ->
  [ diagnostics
  , diagnostics_warnings
  , diagnostics_errors
  ].

-spec includes() -> [atom()].
includes() ->
  [ code_navigation
  , transitive
  , definition
  , diagnostics
  , rename
  ].

%% @doc Index a file and produce the respective config entries
%%
%%      Given an identifier representing a source or include file,
%%      index it and produce a config containing the respective path,
%%      uri and text to simplify accessing this information from test
%%      cases.
-spec index_file(binary(), file_type(), atom()) -> [{atom(), any()}].
index_file(RootPath, Type, Id) ->
  BinaryId = atom_to_binary(Id, utf8),
  Ext = extension(Type),
  Dir = directory(Type),
  Path = filename:join([RootPath, Dir, <<BinaryId/binary, Ext/binary>>]),
  {ok, Uri} = els_indexing:index_file(Path),
  {ok, Text} = file:read_file(Path),
  ConfigId = config_id(Id, Type),
  [ {atoms_append(ConfigId, '_path'), Path}
  , {atoms_append(ConfigId, '_uri'), Uri}
  , {atoms_append(ConfigId, '_text'), Text}
  ].

-spec config_id(atom(), file_type()) -> atom().
config_id(Id, src) -> Id;
config_id(Id, test) -> Id;
config_id(Id, include) -> list_to_atom(atom_to_list(Id) ++ "_h");
config_id(Id, escript) -> list_to_atom(atom_to_list(Id) ++ "_escript").

-spec directory(file_type()) -> binary().
directory(src) ->
  <<"src">>;
directory(test) ->
  <<"test">>;
directory(include) ->
  <<"include">>;
directory(escript) ->
  <<"src">>.

-spec extension(file_type()) -> binary().
extension(src) ->
  <<".erl">>;
extension(test) ->
  <<".erl">>;
extension(include) ->
  <<".hrl">>;
extension(escript) ->
  <<".escript">>.

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
