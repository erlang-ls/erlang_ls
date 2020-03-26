-module(els_mock_background_job).

-export([ setup/0
        , teardown/0
        , subscribe/0
        , wait_until_complete/0
        ]).

-spec setup() -> ok.
setup() ->
  meck:new(els_background_job, [passthrough, no_link]).

-spec teardown() -> ok.
teardown() ->
  meck:unload(els_background_job).

-spec subscribe() -> ok.
subscribe() ->
  Self = self(),
  meck:expect( els_background_job
             , new
             , fun(Config) ->
                   OnComplete = fun(Result) ->
                                    Self ! {on_complete, Result}
                                end,
                   meck:passthrough([Config#{on_complete => OnComplete}])
               end
             ),
  ok.

-spec wait_until_complete() -> any().
wait_until_complete() ->
  receive
    {on_complete, Result} ->
      Result
  end.
