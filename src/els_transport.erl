-module(els_transport).

-callback start_listener(pid()) -> {ok, pid()}.
-callback init(any()) -> any().
-callback send(any(), binary()) -> ok.
