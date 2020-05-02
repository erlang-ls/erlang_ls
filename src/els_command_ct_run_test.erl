-module(els_command_ct_run_test).

-export([ execute/1, task/2 ]).

-include("erlang_ls.hrl").

-spec execute(map()) -> ok.
execute(#{ <<"module">>   := M
         , <<"function">> := F
         , <<"arity">>    := A
         , <<"line">>     := Line
         , <<"uri">>      := Uri
         }) ->
  Msg = io_lib:format("Running Common Test [mfa=~s:~s/~p]", [M, F, A]),
  lager:info(Msg, []),
  Title = unicode:characters_to_binary(Msg),
  Suite = els_uri:module(Uri),
  Case = binary_to_atom(F, utf8),
  Config = #{ task => fun ?MODULE:task/2
            , entries => [{Uri, Line, Suite, Case}]
            , title => Title
            , show_percentages => false
            },
  {ok, _Pid} = els_background_job:new(Config),
  ok.

-spec task({uri(), pos_integer(), atom(), atom()}, any()) -> ok.
task({Uri, Line, Suite, Case}, _State) ->
  case run_test(Suite, Case) of
    {ok, IO} ->
      lager:info("CT Test passed", []),
      publish_result(Uri, Line, ?DIAGNOSTIC_INFO, IO);
    {Error, IO} ->
      Message = els_utils:to_binary(io_lib:format("~p", [Error])),
      lager:info("CT Test failed [error=~p] [io=~p]", [Error, IO]),
      publish_result(Uri, Line, ?DIAGNOSTIC_ERROR, Message)
  end.

-spec publish_result( uri()
                    , pos_integer()
                    , els_diagnostics:severity()
                    , binary()) -> ok.
publish_result(Uri, Line, Severity, Message) ->
  Range = els_protocol:range(#{from => {Line, 1}, to => {Line + 1, 1}}),
  Source = <<"Common Test">>,
  D = els_diagnostics:make_diagnostic(Range, Message, Severity, Source),
  els_diagnostics_provider:publish(Uri, [D]),
  ok.

-spec run_test(atom(), atom()) -> {ok, binary()} | {error, any()}.
run_test(Suite, Case) ->
  Module = els_config_ct_run_test:get_module(),
  Function = els_config_ct_run_test:get_function(),
  els_build_server:rpc_call(Module, Function, [Suite, Case], infinity).
