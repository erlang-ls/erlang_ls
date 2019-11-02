-module(erlang_ls_transport).

-callback init(any()) -> any().
-callback recv(any()) -> binary().
-callback send(any(), binary()) -> ok.
-callback close(any()) -> ok.
