-module(els_command_ct_run_test).

-export([ execute/1 ]).

%% Invoked from the Common Test Hook
-export([ publish_result/4, run_test/2, ct_run_test/1 ]).

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
  Config = #{ task        => fun ?MODULE:run_test/2
            , entries     => [{Uri, Line, F}]
            , title       => Title
            },
  {ok, _Pid} = els_background_job:new(Config),
  ok.

-spec run_test({uri(), pos_integer(), binary()}, any()) -> ok.
run_test({Uri, Line, TestCase}, _State) ->
  Opts = [ {suite,        [binary_to_list(els_uri:path(Uri))]}
         , {testcase,     [binary_to_atom(TestCase, utf8)]}
         , {include,      els_config:get(include_paths)}
         , {auto_compile, true}
         , {ct_hooks,     [{els_cth, #{uri => Uri, line => Line}}]}
         ],
  Result = ?MODULE:ct_run_test(Opts),
  lager:info("CT Result: ~p", [Result]),
  case Result of
    {N, 0, {0, 0}} when N > 0 ->
      publish_result(Uri, Line, ?DIAGNOSTIC_INFO, <<"Test passed!">>);
    _ ->
      %% In case of skipped or failed tests, the result is published
      %% by the Common Test hook, so nothing to do here.
      ok
  end.

-spec publish_result( uri()
                    , pos_integer()
                    , els_diagnostics:severity()
                    , binary()) -> ok.
publish_result(Uri, Line, Severity, Message) ->
  Range = els_protocol:range(#{from => {Line, 1}, to => {Line + 1, 1}}),
  Source = <<"Common Test">>,
  D = els_diagnostics:make_diagnostic(Range, Message, Severity, Source),
  els_diagnostics:publish(Uri, [D]),
  ok.

-spec ct_run_test([any()]) -> any().
ct_run_test(Opts) ->
  ct:run_test(Opts).
