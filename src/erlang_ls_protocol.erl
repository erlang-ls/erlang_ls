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
               , text
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.
-type request()      :: #{}.
-type response()     :: #{}.
-type result()       :: #{}.

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
connected(info, {tcp, Socket, Data}, #state{ socket = Socket } = State) ->
  handle_request(Data, State);
connected(info, {tcp_closed, _Socket}, _State) ->
  {stop, normal};
connected(info, {tcp_error, _, Reason}, _State) ->
  {stop, Reason}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec handle_request(binary(), state()) -> any().
handle_request(Data, #state{ socket    = Socket
                           , transport = Transport
                           } = State) ->
  Request   = parse_data(Data),
  Method    = maps:get(<<"method">>, Request),
  Params    = maps:get(<<"params">>, Request),
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
  Text           = maps:get(<<"text">>        , TextDocument),
  {State#state{ text = Text }};
handle_request(<<"textDocument/didChange">>, Params, State) ->
  ContentChanges = maps:get(<<"contentChanges">>, Params),
  case ContentChanges of
    []                      -> {State};
    [#{<<"text">> := Text}] -> {State#state{ text = Text }}
  end;
handle_request(<<"textDocument/hover">>, _Params, State) ->
  {null, State};
handle_request(<<"textDocument/completion">>, Params, #state{ text = Text
                                                            } = State) ->
  Position    = maps:get(<<"position">> , Params),
  Line        = maps:get(<<"line">>     , Position),
  Character   = maps:get(<<"character">>, Position),
  Completions = get_completions(Text, Line, Character),
  Result      = [#{label => C} || C <- Completions],
  {Result, State};
handle_request(Method, _Params, State) ->
  erlang:display({not_implemented, Method}),
  {State}.

-spec parse_data(binary()) -> request().
parse_data(Data) ->
  {_Headers, Body} = cow_http:parse_headers(Data),
  jsx:decode(Body, [return_maps]).

-spec build_response(integer(), result()) -> response().
build_response(RequestId, Result) ->
  #{ jsonrpc => ?JSONRPC_VSN
   , result  => Result
   , id      => RequestId
   }.

-spec reply(any(), module(), response()) -> ok.
reply(Socket, Transport, Response) ->
  Body    = jsx:encode(Response),
  Headers = io_lib:format("Content-Length: ~p\r\n", [byte_size(Body)]),
  Data    = [Headers, "\r\n", Body],
  Transport:send(Socket, Data).

-spec get_completions(binary(), integer(), integer()) -> [binary()].
get_completions(Text, Line, Character) ->
  LineText        = get_line_text(Text, Line),
  LineBeforeChar  = binary:part(LineText, {0, Character - 1}),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(LineBeforeChar)),
  [H| _] = lists:reverse(Tokens),
  Info = case H of
           {atom, _, Atom} ->
             try Atom:module_info(exports)
             catch _:_ -> []
             end;
           _ ->
             []
         end,
  [function_name_to_binary(M, A) || {M, A} <- Info].

-spec get_line_text(binary(), integer()) -> binary().
get_line_text(Text, Line) ->
  Lines = binary:split(Text, <<"\n">>, [global]),
  lists:nth(Line + 1, Lines).

-spec function_name_to_binary(module(), non_neg_integer()) -> binary().
function_name_to_binary(Module, Arity) ->
  list_to_binary(io_lib:format("~p/~p", [Module, Arity])).
