-module(els_mock_build_server).

-export([ setup/0
        , teardown/0
        ]).

-spec setup() -> ok.
setup() ->
  meck:new(els_build_server, [no_link, passthrough]),
  meck:expect(els_build_server, connect, 0, ok),
  meck:expect(els_build_server, request, 3, fun request/3).

-spec teardown() -> ok.
teardown() ->
  meck:unload(els_build_server).

%% Internal
request(<<"workspace/targets">>, _Params, _Timeout) ->
  #{targets => [default]};
request(<<"buildTargets/sources">>, _Params, _Timeout) ->
  Items = filename:join([ code:priv_dir(erlang_ls), "code_navigation"]),
  #{items => Items}.
