%%==============================================================================
%% The Erlang Language Server
%%==============================================================================
-module(erlang_ls_server).

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
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, {socket, buffer}).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%==============================================================================
%% ranch_protocol callbacks
%%==============================================================================
-spec start_link(ranch:ref(), any(), module(), any()) -> {ok, pid()}.
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
  ok = Transport:setopts(Socket, [ {active, once}
                                 , {packet, 0}
                                 ]),
  gen_statem:enter_loop( ?MODULE
                       , []
                       , connected
                       , #state{ socket = Socket
                               , buffer = <<>>
                               }
                       ).

-spec code_change(any(), atom(), state(), any()) -> {ok, atom(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.

-spec terminate(any(), atom(), state()) -> any().
terminate(_Reason, _StateName, #state{socket = Socket}) ->
  gen_tcp:close(Socket),
  ok.

%%==============================================================================
%% gen_statem State Functions
%%==============================================================================
-spec connected(gen_statem:event_type(), any(), state()) -> any().
connected(info, {tcp, Socket, Packet}, #state{ socket = Socket
                                             , buffer = Buffer
                                             } = State) ->
  lager:debug("[SERVER] TCP Packet [buffer=~p] [packet=~p] ", [Buffer, Packet]),
  Data = <<Buffer/binary, Packet/binary>>,
  {Requests, NewBuffer} = erlang_ls_jsonrpc:split(Data, [return_maps]),
  [handle_request(Socket, Request) || Request <- Requests],
  inet:setopts(Socket, [{active, once}]),
  {keep_state, State#state{ buffer = NewBuffer }};
connected(info, {tcp_closed, _Socket}, _State) ->
  {stop, normal};
connected(info, {'EXIT', _, normal}, _State) ->
  keep_state_and_data;
connected(info, {tcp_error, _, Reason}, _State) ->
  {stop, Reason};
connected(cast, {notification, M, P}, State) ->
  send_notification(State#state.socket, M, P),
  keep_state_and_data.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(any(), map()) -> ok.
handle_request(Socket, Request) ->
  Method    = maps:get(<<"method">>, Request),
  Params    = maps:get(<<"params">>, Request),
  case handle_method(Method, Params) of
    {response, Result} ->
      RequestId = maps:get(<<"id">>, Request),
      Response = erlang_ls_protocol:response(RequestId, Result),
      lager:debug("[SERVER] Sending response [response=~p]", [Response]),
      gen_tcp:send(Socket, Response);
    {} ->
      lager:debug("[SERVER] No response", []),
      ok;
    {notification, M, P} ->
      send_notification(Socket, M, P)
  end.

-spec handle_method(binary(), map()) ->
  {response, map()} | {} | {notification, binary(), map()}.
handle_method(<<"initialize">>, Params) ->
  RootUri = maps:get(<<"rootUri">>, Params),
  ok = erlang_ls_buffer_server:set_root_uri(RootUri),
  Result = #{ capabilities =>
                #{ hoverProvider => false
                 , completionProvider =>
                     #{ resolveProvider => false
                      , triggerCharacters => [<<":">>, <<"#">>]
                      }
                 , textDocumentSync => 1
                 , definitionProvider => true
                 }
            },
  {response, Result};
handle_method(<<"initialized">>, _) ->
  {};
handle_method(<<"textDocument/didOpen">>, Params) ->
  ok = erlang_ls_text_synchronization:did_open(Params),
  {};
handle_method(<<"textDocument/didChange">>, Params) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  TextDocument   = maps:get(<<"textDocument">>  , Params),
  Uri            = maps:get(<<"uri">>           , TextDocument),
  case ContentChanges of
    []                      -> ok;
    [#{<<"text">> := Text}] ->
      {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
      ok = erlang_ls_buffer:set_text(Buffer, Text)
  end,
  {};
handle_method(<<"textDocument/hover">>, _Params) ->
  {response, null};
handle_method(<<"textDocument/completion">>, Params) ->
  Position     = maps:get(<<"position">> , Params),
  Line         = maps:get(<<"line">>     , Position),
  Character    = maps:get(<<"character">>, Position),
  TextDocument = maps:get(<<"textDocument">>  , Params),
  Uri          = maps:get(<<"uri">>      , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  Result       = erlang_ls_buffer:get_completions(Buffer, Line, Character),
  {response, Result};
handle_method(<<"textDocument/didSave">>, Params) ->
  spawn(erlang_ls_text_synchronization, did_save, [Params, self()]),
  {};
handle_method(<<"textDocument/didClose">>, Params) ->
  ok = erlang_ls_text_synchronization:did_close(Params),
  {};
handle_method(<<"textDocument/definition">>, Params) ->
  Position     = maps:get(<<"position">>    , Params),
  Line         = maps:get(<<"line">>        , Position),
  Character    = maps:get(<<"character">>   , Position),
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  case erlang_ls_buffer:get_element_at_pos(Buffer, Line + 1, Character + 1) of
    [POI|_] ->
      Filename = erlang_ls_uri:path(Uri),
      case erlang_ls_code_navigation:goto_definition(Filename, POI) of
        {error, _Error} ->
          {response, null};
        {ok, FullName, Range} ->
          {response, #{ uri => erlang_ls_uri:uri(FullName)
                      , range => erlang_ls_protocol:range(Range)
                      }}
      end;
    [] ->
      {response, null}
  end;
handle_method(<<"shutdown">>, _Params) ->
  %% TODO: keep in the state that we got a shutdown
  {response, null};
handle_method(<<"exit">>, _Params) ->
  %% TODO: exit with 1 if shutdown wasn't sent before
  erlang:halt(0);
handle_method(Method, _Params) ->
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  Message = <<"Method not implemented: ", Method/binary>>,
  Method1 = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  {notification, Method1, Params}.

-spec send_notification(any(), binary(), map()) -> ok.
send_notification(Socket, Method, Params) ->
  Notification = erlang_ls_protocol:notification(Method, Params),
  lager:debug("[SERVER] Sending notification [notification=~p]", [Notification]),
  gen_tcp:send(Socket, Notification).
