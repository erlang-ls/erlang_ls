-module(els_fake_stdio).

-export([ start/0
        , connect/2
        , loop/1
        ]).

-record(state, { buffer = <<>> :: binary()
               , connected     :: pid()
               , pending = []  :: [any()]
               }).

-type state() :: #state{}.

-spec start() -> pid().
start() ->
  proc_lib:spawn_link(?MODULE, loop, [#state{}]).

-spec connect(pid(), pid()) -> ok.
connect(Pid, IoDevice) ->
  Pid ! {connect, IoDevice},
  ok.

-spec loop(state()) -> ok.
loop(State0) ->
  State1 = process_pending(State0),
  receive
    {connect, IoDevice} ->
      loop(State1#state{connected = IoDevice});
    {custom_request, From, Ref, Request} ->
      State = handle_locally(From, Ref, Request, State1),
      loop(State);
    {io_request, From, Ref, Request} ->
      State = dispatch(From, Ref, Request, State1),
      loop(State)
  end.

-spec dispatch(pid(), any(), any(), state()) -> ok.
dispatch(From, Ref, Request, State0) ->
  case is_custom(Request) of
    true  -> redirect(From, Ref, Request, State0);
    false -> handle_locally(From, Ref, Request, State0)
  end.

-spec is_custom(any()) -> boolean().
is_custom({put_chars, _Encoding, _Chars}) ->
  true;
is_custom({put_chars, _Encoding, _M, _F, _Args}) ->
  true;
is_custom(_) ->
  false.

-spec redirect(pid(), any(), any(), state()) -> state().
redirect(From, Ref, Request, State) ->
  Connected = State#state.connected,
  Connected ! {custom_request, From, Ref, Request},
  State.

-spec handle_locally(pid(), any(), any(), state()) -> state().
handle_locally(From, Ref, Request, State0) ->
  case handle_request(Request, State0) of
    {noreply, State} ->
      pending(From, Ref, Request, State);
    {reply, Reply, State} ->
      reply(From, Ref, Reply),
      State
  end.

-spec handle_request(any(), state()) ->
  {reply, any(), state()} | {noreply, state()}.
handle_request({setopts, _Opts}, State) ->
  {reply, ok, State};
handle_request({put_chars, Encoding, M, F, Args}, State0) ->
  Chars = apply(M, F, Args),
  handle_request({put_chars, Encoding, Chars}, State0);
handle_request({put_chars, Encoding, Chars}, State0) ->
  EncodedChars = unicode:characters_to_list(Chars, Encoding),
  CharsBin     = unicode:characters_to_binary(EncodedChars),
  Buffer       = State0#state.buffer,
  State        = State0#state{buffer = <<Buffer/binary, CharsBin/binary>>},
  {reply, ok, State};
handle_request({get_line, _Encoding, _Prompt}, State0) ->
  case binary:split(State0#state.buffer, <<"\n">>, [trim]) of
    [Line0, Rest] ->
      Line = string:trim(Line0),
      {reply, <<Line/binary, "\n">>, State0#state{buffer = Rest}};
    _ ->
      {noreply, State0}
  end;
handle_request({get_chars, Encoding, _Prompt, Count}, State) ->
  handle_request({get_chars, Encoding, Count}, State);
handle_request({get_chars, _Encoding, Count}, State0) ->
  case State0#state.buffer of
    <<Data:Count/binary, Rest/binary>> ->
      {reply, Data, State0#state{buffer = Rest}};
    _ ->
      {noreply, State0}
  end.

-spec reply(pid(), any(), any()) -> any().
reply(From, ReplyAs, Reply) ->
    From ! {io_reply, ReplyAs, Reply}.

-spec pending(pid(), any(), any(), state()) -> state().
pending(From, Ref, Request, #state{pending = Pending} = State) ->
  State#state{pending = [{From, Ref, Request} | Pending]}.

-spec process_pending(state()) -> state().
process_pending(#state{pending = Pending} = State) ->
  FoldFun = fun({From, Ref, Request}, Acc) ->
                handle_locally(From, Ref, Request, Acc)
            end,
  lists:foldl(FoldFun, State#state{pending = []}, Pending).
