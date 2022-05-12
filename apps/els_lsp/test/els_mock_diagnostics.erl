-module(els_mock_diagnostics).

-export([
    setup/0,
    teardown/0,
    subscribe/0,
    wait_until_complete/0
]).

-spec setup() -> ok.
setup() ->
    meck:new(els_diagnostics_provider, [passthrough, no_link]).

-spec teardown() -> ok.
teardown() ->
    meck:unload(els_diagnostics_provider).

-spec subscribe() -> ok.
subscribe() ->
    Self = self(),
    meck:expect(
        els_diagnostics_provider,
        publish,
        fun(Uri, Diagnostics) ->
            Self ! {on_complete, Diagnostics},
            meck:passthrough([Uri, Diagnostics])
        end
    ),
    ok.

-spec wait_until_complete() -> [els_diagnostics:diagnostic()].
wait_until_complete() ->
    wait_until_complete(els_diagnostics:enabled_diagnostics(), []).

-spec wait_until_complete(
    [els_diagnostics:diagnostic_id()],
    [els_diagnostics:diagnostic()]
) ->
    [els_diagnostics:diagnostic()].
wait_until_complete([], Diagnostics) ->
    Diagnostics;
wait_until_complete([_ | Rest], _Diagnostics) ->
    receive
        {on_complete, Diagnostics} ->
            wait_until_complete(Rest, Diagnostics)
    end.
