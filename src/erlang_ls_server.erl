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
-record(state, {socket, data, length}).

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
  ok = Transport:setopts(Socket, [{active, true}, {packet, 0}]),
  gen_statem:enter_loop( ?MODULE
                       , []
                       , connected
                       , #state{ socket = Socket
                               , data   = <<>>
                               , length = undefined
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
connected(info, {tcp, Socket, NewData}, #state{ socket = Socket
                                              , data   = OldData
                                              , length = Length
                                              } = State) ->
  Data = <<OldData/binary, NewData/binary>>,
  case Length =/= undefined andalso Length < byte_size(Data) of
    true ->
      {keep_state, State#state{data = Data}};
    false ->
      Rest = handle_requests(Socket, Data),
      {keep_state, State#state{data = Rest}}
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
-spec handle_requests(any(), binary()) -> binary().
handle_requests(Socket, Data) ->
  {Headers, Payload} = cow_http:parse_headers(Data),
  BinLength          = proplists:get_value(<<"content-length">>, Headers),
  L                  = binary_to_integer(BinLength),
  <<Body:L/binary, Rest/binary>> = Payload,
  Request = jsx:decode(Body, [return_maps]),
  case handle_request(Socket, Request) of
    {Result} ->
      RequestId = maps:get(<<"id">>, Request),
      ok = erlang_ls_protocol:response(Socket, RequestId, Result);
    {} ->
      ok
  end,
  case byte_size(Rest) > 0 of
    true ->
      handle_requests(Socket, Rest);
    false ->
      Rest
  end.

-spec handle_request(any(), map()) -> ok.
handle_request(Socket, Request) ->
  Method = maps:get(<<"method">>, Request),
  Params = maps:get(<<"params">>, Request),
  lager:debug("[Handling request] [method=~s] [params=~p]", [Method, Params]),
  handle_request(Socket, Method, Params).

-spec handle_request(any(), binary(), map()) -> {any()} | {}.
handle_request(_Socket, <<"initialize">>, _Params) ->
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
  {Result};
handle_request(_Socket, <<"initialized">>, _) ->
  {};
handle_request(_Socket, <<"textDocument/didOpen">>, Params) ->
  ok = erlang_ls_text_synchronization:did_open(Params),
  {};
handle_request(_Socket, <<"textDocument/didChange">>, Params) ->
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
handle_request(_Socket, <<"textDocument/hover">>, _Params) ->
  {null};
handle_request(_Socket, <<"textDocument/completion">>, Params) ->
  Position     = maps:get(<<"position">> , Params),
  Line         = maps:get(<<"line">>     , Position),
  Character    = maps:get(<<"character">>, Position),
  TextDocument = maps:get(<<"textDocument">>  , Params),
  Uri          = maps:get(<<"uri">>      , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  Result       = erlang_ls_buffer:get_completions(Buffer, Line, Character),
  {Result};
handle_request(Socket, <<"textDocument/didSave">>, Params) ->
  ok = erlang_ls_text_synchronization:did_save(Socket, Params),
  {};
handle_request(_Socket, <<"textDocument/definition">>, Params) ->
  Position     = maps:get(<<"position">>    , Params),
  Line         = maps:get(<<"line">>        , Position),
  Character    = maps:get(<<"character">>   , Position),
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  {ok, Buffer} = erlang_ls_buffer_server:get_buffer(Uri),
  {M, F, A}    = erlang_ls_buffer:get_mfa(Buffer, Line, Character),
  Which = code:which(M),
  Source = list_to_binary(proplists:get_value( source
                                             , M:module_info(compile))),
  DefUri = <<"file://", Source/binary>>,
  {ok, {_, [{abstract_code, {_, AC}}]}} = beam_lib:chunks(Which, [abstract_code]),
  Result = case [ AL || {function, AL, AF, AA, _} <- AC, F =:= AF, A =:= AA] of
             [DefLine] ->
               #{ uri => DefUri
                , range => erlang_ls_protocol:range(erl_anno:line(DefLine) - 1)
                };
             [] ->
               null
           end,
  {Result};
handle_request(Socket, RequestMethod, _Params) ->
  Message = <<"Method not implemented: ", RequestMethod/binary>>,
  Method  = <<"window/showMessage">>,
  Params  = #{ type    => ?MESSAGE_TYPE_INFO
             , message => Message
             },
  erlang_ls_protocol:notification(Socket, Method, Params),
  lager:warning("[Method not implemented] [method=~s]", [Method]),
  {}.
