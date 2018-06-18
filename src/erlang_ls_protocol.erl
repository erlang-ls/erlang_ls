%%==============================================================================
%% The Language Server Protocol Implementation
%%==============================================================================
-module(erlang_ls_protocol).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(ranch_protocol).
-behaviour(gen_statem).

%%==============================================================================
%% Exports
%%==============================================================================

%% ranch_protocol callbacks
-export([ start_link/4 ]).

%% gen_statem callbacks
-export([ callback_mode/0
        , code_change/4
        , init/1
        , terminate/3
        ]).

%% gen_statem state functions
-export([ connected/3
        ]).

%%==============================================================================
%% Defines
%%==============================================================================
-define(JSONRPC_VSN, <<"2.0">>).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { socket
               , transport
               , length
               , body
               , buffers = []
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.
-type request()      :: #{}.
-type response()     :: #{}.
-type result()       :: #{}.
-type message()      :: #{}.
-type params()       :: #{}.
-type diagnostic()   :: #{}.
-type range()        :: #{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================
-spec start_link(ramnch:ref(), any(), module(), any()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
  {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%==============================================================================
%% gen_statem callbacks
%%==============================================================================
-spec callback_mode() -> state_functions.
callback_mode() ->
  state_functions.

-spec init({ranch:ref(), any(), module(), any()}) -> no_return().
init({Ref, Socket, Transport, _Opts}) ->
  ok = ranch:accept_ack(Ref),
  ok = Transport:setopts(Socket, [{active, true}, {packet, 0}]),
  gen_statem:enter_loop( ?MODULE
                       , []
                       , connected
                       , #state{ socket    = Socket
                               , transport = Transport
                               }
                       ).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec terminate(any(), atom(), state()) -> any().
terminate(_Reason, _StateName, #state{ socket    = Socket
                                     , transport = Transport
                                     }) ->
  Transport:close(Socket),
  ok.

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================
-spec connected(gen_statem:event_type(), any(), state()) -> any().
connected(info, {tcp, Socket, TcpData}, #state{ socket = Socket } = State) ->
  case State#state.length of
    undefined ->
      {Headers, Body} = cow_http:parse_headers(TcpData),
      BinLength       = proplists:get_value( <<"content-length">>, Headers),
      Length          = binary_to_integer(BinLength),
      handle_body_part(State#state{ length = Length, body = Body });
    _ ->
      OldBody = State#state.body,
      handle_body_part(State#state{ body = <<OldBody/binary, TcpData/binary>> })
  end;
connected(info, {tcp_closed, _Socket}, _State) ->
  {stop, normal};
connected(info, {'EXIT', _, normal}, _State) ->
  keep_state_and_data;
connected(info, {tcp_error, _, Reason}, _State) ->
  {stop, Reason}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_body_part(state()) -> any().
handle_body_part(#state{body = Body, length = Length} = State) ->
  case byte_size(Body) < Length of
    true  -> {keep_state, State};
    false -> handle_request(State#state{ length = undefined })
  end.

-spec handle_request(state()) -> any().
handle_request(#state{ socket    = Socket
                     , transport = Transport
                     , body      = Body
                     } = State) ->
  Request   = parse_data(Body),
  Method    = maps:get(<<"method">>, Request),
  Params    = maps:get(<<"params">>, Request),
  lager:debug("[Handling request] [method=~s] [params=~p]", [Method, Params]),
  case handle_request(Method, Params, State) of
    {Result, NewState} ->
      RequestId = maps:get(<<"id">>, Request),
      Response  = build_response(RequestId, Result),
      reply(Socket, Transport, Response),
      {keep_state, NewState};
    {NewState}         ->
      {keep_state, NewState}
  end.

-spec handle_request(binary(), map(), state()) ->
  {result(), state()} | {state()}.
handle_request(<<"initialize">>, _Params, State) ->
  Result = #{ capabilities =>
                #{ completionProvider =>
                     #{ resolveProvider => false
                      , triggerCharacters => [<<":">>]
                      }
                 , textDocumentSync => 1
                 }
            },
  {Result, State};
handle_request(<<"initialized">>, _, State) ->
  {State};
handle_request(<<"textDocument/didOpen">>, Params, State) ->
  TextDocument   = maps:get(<<"textDocument">>, Params),
  Uri            = maps:get(<<"uri">>         , TextDocument),
  Text           = maps:get(<<"text">>        , TextDocument),
  {ok, Pid}      = supervisor:start_child(erlang_ls_sup, [Text]),
  {State#state{ buffers = [{Uri, Pid}|State#state.buffers] }};
handle_request(<<"textDocument/didChange">>, Params, State) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    []                      -> ok;
    [#{<<"text">> := Text}] ->
      Pid = proplists:get_value(Uri, State#state.buffers),
      erlang_ls_buffer:set_text(Pid, Text)
  end,
  {State};
handle_request(<<"textDocument/hover">>, _Params, State) ->
  {null, State};
handle_request(<<"textDocument/completion">>, Params, State) ->
  Position     = maps:get(<<"position">> , Params),
  Line         = maps:get(<<"line">>     , Position),
  Character    = maps:get(<<"character">>, Position),
  TextDocument = maps:get(<<"textDocument">>  , Params),
  Uri          = maps:get(<<"uri">>      , TextDocument),
  Pid          = proplists:get_value(Uri, State#state.buffers),
  Completions  = erlang_ls_buffer:get_completions(Pid, Line, Character),
  Result       = [#{label => C} || C <- Completions],
  {Result, State};
handle_request(<<"textDocument/didSave">>, Params, State) ->
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  CDiagnostics = get_compilation_diagnostics(Uri),
  DDiagnostics =  get_dialyzer_diagnostics(Uri),
  Params1  = #{ uri => Uri
              , diagnostics => CDiagnostics ++ DDiagnostics
              },
  Message = build_message(<<"textDocument/publishDiagnostics">>, Params1),
  reply(State#state.socket, State#state.transport, Message),
  {State};
handle_request(Method, _Params, State) ->
  Text    = <<"Method not implemented: ", Method/binary>>,
  Params  = #{ type    => 3
             , message => Text
             },
  Message = build_message(<<"window/showMessage">>, Params),
  reply(State#state.socket, State#state.transport, Message),
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  {State}.

-spec parse_data(binary()) -> request().
parse_data(Body) ->
  jsx:decode(Body, [return_maps]).

-spec build_message(binary(), params()) -> message().
build_message(Method, Params) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , method  => Method
   , params  => Params
   }.

-spec build_response(integer(), result()) -> response().
build_response(RequestId, Result) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , result  => Result
   , id      => RequestId
   }.

-spec build_range(integer()) -> range().
build_range(Line) ->
  #{ start => #{line => Line, character => 0}
   , 'end' => #{line => Line, character => 0}
   }.

-spec build_diagnostic(range(), binary(), integer()) -> diagnostic().
build_diagnostic(Range, Message, Severity) ->
  #{ range    => Range
   , message  => Message
   , severity => Severity
   }.

-spec get_compilation_diagnostics(binary()) -> [diagnostic()].
get_compilation_diagnostics(Uri) ->
  <<"file://", Path/binary>> = Uri,
  CompileOpts = [debug_info, return_warnings, return_errors],
  case compile:file(binary_to_list(Path), CompileOpts) of
    {ok, _, WS} ->
      build_compilation_diagnostics(WS, 1);
    {error, ES, WS} ->
      build_compilation_diagnostics(WS, 1) ++
        build_compilation_diagnostics(ES, 2)
  end.

-spec build_compilation_diagnostics(any(), integer()) -> [diagnostic()].
build_compilation_diagnostics(List, Severity) ->
  lists:flatten([[ build_compilation_diagnostic(Line, Module, Desc, Severity)
                   || {Line, Module, Desc} <- Info]
                 || {_Filename, Info } <- List]).

-spec build_compilation_diagnostic(integer(), module(), string(), integer()) ->
  diagnostic().
build_compilation_diagnostic(Line, Module, Desc, Severity) ->
  Range   = build_range(Line),
  Message = list_to_binary(lists:flatten(Module:format_error(Desc))),
  build_diagnostic(Range, Message, Severity).

-spec get_dialyzer_diagnostics(binary()) -> [diagnostic()].
get_dialyzer_diagnostics(Uri) ->
  <<"file://", Path/binary>> = Uri,
    WS = try dialyzer:run([{files, [binary_to_list(Path)]}, {from, src_code}])
         catch _:_ ->
                 []
         end,
  [build_dialyzer_diagnostic(W) || W <- WS].

-spec build_dialyzer_diagnostic(any()) ->
  diagnostic().
build_dialyzer_diagnostic({_, {_, Line}, _} = Warning) ->
  Range   = build_range(Line),
  Message = list_to_binary(lists:flatten(dialyzer:format_warning(Warning))),
  build_diagnostic(Range, Message, 2).

-spec reply(any(), module(), response()) -> ok.
reply(Socket, Transport, Response) ->
  Body    = jsx:encode(Response),
  Headers = io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]),
  Data    = [Headers, "\r\n", Body],
  Transport:send(Socket, Data).
