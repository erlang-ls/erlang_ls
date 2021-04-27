%% @doc Buffer textedits to avoid spamming indexing for every keystroke
%%
%% FIXME: Currently implemented as a simple process.
%%        Might be nicer to rewrite this as a gen_server
%% FIXME: Edits are bottlenecked by this single process, so this could slow
%%        down indexing when making changes in multiple files, this could be
%%        mitigated by using a worker pool or spawning a process per Uri.
-module(els_index_buffer).

-export([ start/0
        , stop/0
        , apply_edits_async/2
        , flush/1
        ]).

-include_lib("kernel/include/logger.hrl").
-include("els_lsp.hrl").

-define(SERVER, ?MODULE).

-spec start() -> {ok, pid()} | {error, _}.
start() ->
  case whereis(?SERVER) of
    undefined ->
      Pid = proc_lib:spawn_link(fun loop/0),
      true = erlang:register(?SERVER, Pid),
      ?LOG_INFO("[~p] Started.", [?SERVER]),
      {ok, Pid};
    Pid ->
      {error, {already_started, Pid}}
  end.

-spec stop() -> ok.
stop() ->
  ?SERVER ! stop,
  erlang:unregister(?SERVER),
  ?LOG_INFO("[~p] Stopped.", [?SERVER]),
  ok.

-spec apply_edits_async(uri(), [els_text:edit()]) -> ok.
apply_edits_async(Uri, Edits) ->
  ?SERVER ! {apply_edits, Uri, Edits},
  ok.

-spec flush(uri()) -> ok.
flush(Uri) ->
  Ref = make_ref(),
  ?SERVER ! {flush, self(), Ref, Uri},
  receive
    {Ref, done} ->
      ok
  end.

-spec loop() -> ok.
loop() ->
  receive
    stop -> ok;
    {apply_edits, Uri, Edits} ->
      try
        do_apply_edits(Uri, Edits)
      catch E:R:St ->
          ?LOG_ERROR("[~p] Crashed while applying edits ~p: ~p",
                     [?SERVER, Uri, {E, R, St}])
      end,
      loop();
    {flush, Pid, Ref, Uri} ->
      try
        do_flush(Uri)
      catch E:R:St ->
          ?LOG_ERROR("[~p] Crashed while flushing ~p: ~p",
                     [?SERVER, Uri, {E, R, St}])
      end,
      Pid ! {Ref, done},
      loop()
  end.

-spec do_apply_edits(uri(), [els_text:edit()]) -> ok.
do_apply_edits(Uri, Edits0) ->
  ?LOG_DEBUG("[~p] Processing index request for ~p", [?SERVER, Uri]),
  {ok, [#{text := Text0}]} = els_dt_document:lookup(Uri),
  ?LOG_DEBUG("[~p] Apply edits: ~p", [?SERVER, Edits0]),
  Text1 = els_text:apply_edits(Text0, Edits0),
  Text = receive_all(Uri, Text1),
  ?LOG_DEBUG("[~p] Started indexing ~p", [?SERVER, Uri]),
  {Duration, ok} = timer:tc(fun() -> els_indexing:index(Uri, Text, 'deep') end),
  ?LOG_DEBUG("[~p] Done indexing ~p [duration: ~pms]",
             [?SERVER, Uri, Duration div 1000]),
  ok.

-spec do_flush(uri()) -> ok.
do_flush(Uri) ->
  ?LOG_DEBUG("[~p] Flushing ~p", [?SERVER, Uri]),
  {ok, [#{text := Text0}]} = els_dt_document:lookup(Uri),
  Text = receive_all(Uri, Text0),
  {Duration, ok} = timer:tc(fun() -> els_indexing:index(Uri, Text, 'deep') end),
  ?LOG_DEBUG("[~p] Done flushing ~p [duration: ~pms]",
             [?SERVER, Uri, Duration div 1000]),
  ok.

-spec receive_all(uri(), binary()) -> binary().
receive_all(Uri, Text0) ->
  receive
    {apply_edits, Uri, Edits} ->
      ?LOG_DEBUG("[~p] Received more edits ~p.", [?SERVER, Uri]),
      ?LOG_DEBUG("[~p] Apply edits: ~p", [?SERVER, Edits]),
      Text = els_text:apply_edits(Text0, Edits),
      receive_all(Uri, Text)
  after
    0 -> Text0
  end.
