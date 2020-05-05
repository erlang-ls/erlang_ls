-module(els_mock_build_server).

-include_lib("erlang_bs/include/erlang_bs.hrl").

-export([ setup/0
        , teardown/0
        ]).

-define(RPC_TIMEOUT, 5000).

-spec setup() -> ok.
setup() ->
  meck:new(els_build_server, [no_link, passthrough]),
  meck:expect(els_build_server, connect, 0, ok),
  meck:expect(els_build_server, request, 2, fun request/2),
  meck:expect(els_build_server, request, 3, fun request/3).

-spec teardown() -> ok.
teardown() ->
  meck:unload(els_build_server).

-spec request(binary(), map()) -> map().
request(Method, Params) ->
  request(Method, Params, ?RPC_TIMEOUT).

%% Internal
-spec request(binary(), map(), timeout()) -> map().
request(<<"workspace/targets">>, _Params, _Timeout) ->
  #{targets => [default]};
request(<<"buildTarget/sources">>, _Params, _Timeout) ->
  Path = filename:join([code:priv_dir(erlang_ls), "code_navigation"]),
  Item = #{ target => default
          , sources => #{ uri => els_uri:uri(els_utils:to_binary(Path))
                        , kind => ?SOURCE_ITEM_KIND_DIR
                        , generated => false
                        }
          },
  #{items => [Item]};
request(Method, _Params, _Timeout) ->
  #{not_mocked => Method}.
