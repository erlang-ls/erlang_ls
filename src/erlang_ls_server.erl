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

-define(OTP_INCLUDE_PATH, "/usr/local/Cellar/erlang/21.2.4/lib/erlang/lib").
%% TODO: Implement support for workspaces
-define(ERLANG_LS_PATH, "/Users/robert.aloi/git/github/erlang-ls/erlang_ls").
-define(TEST_APP_PATH, "/Users/robert.aloi/git/github/erlang-ls/test").
-define(DEPS_PATH, "/Users/robert.aloi/git/github/erlang-ls/erlang_ls/_build/debug/lib").

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
handle_method(<<"initialize">>, _Params) ->
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
      {response, definition(Uri, POI)};
    [] ->
      {response, null}
  end;
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

-spec definition(uri(), erlang_ls_parser:poi()) -> null | map().
definition(_Uri, #{ info := {application, {M, F, A}} }) ->
  case annotated_tree(erlang_ls_uri:filename(M)) of
    {ok, Uri, AnnotatedTree} ->
      %% TODO: Abstract this mapping in a function
      Info = {function, {F, A}},
      case erlang_ls_parser:find_poi_by_info(AnnotatedTree, Info) of
        [#{ range := Range }] ->
          %% TODO: Use API to create types
          #{ uri => Uri
           , range => erlang_ls_protocol:range(Range)
           };
        [] ->
          null
      end;
    {error, _Error} ->
      null
  end;
definition(Uri, #{ info := {application, {F, A}} }) ->
  case annotated_tree(erlang_ls_uri:basename(Uri)) of
    {ok, Uri, AnnotatedTree} ->
      %% TODO: Abstract this mapping in a function
      Info = {function, {F, A}},
      case erlang_ls_parser:find_poi_by_info(AnnotatedTree, Info) of
        [#{ range := Range }] ->
          %% TODO: Use API to create types
          #{ uri => Uri
           , range => erlang_ls_protocol:range(Range)
           };
        [] ->
          null
      end;
    {error, _Error} ->
      null
  end;
definition(_Uri, #{ info := {behaviour, Behaviour} }) ->
  case annotated_tree(erlang_ls_uri:filename(Behaviour)) of
    {ok, Uri, _AnnotatedTree} ->
      #{ uri => Uri
         %% TODO: We could point to the module attribute, instead
       , range => erlang_ls_protocol:range(#{ from => {0, 0}
                                            , to   => {0, 0}
                                            })
       };
    {error, _Error} ->
      null
  end;
%% TODO: Eventually search everywhere and suggest a code lens to include a file
%% TODO: Create a function to host the mapping between poi and definition
definition(Uri, #{ info := {macro, Define} }) ->
  Filename = erlang_ls_uri:basename(Uri),
  search(Filename, app_path(), {define, Define});
definition(Uri, #{ info := {record_expr, Record} }) ->
  Filename = erlang_ls_uri:basename(Uri),
  search(Filename, app_path(), {record, Record});
definition(_Uri, _) ->
  null.

-spec annotated_tree(binary()) ->
   {ok, uri(), erlang_ls_parser:syntax_tree()} | {error, any()}.
annotated_tree(Filename) ->
  Path = lists:append( [ app_path() , deps_path() , otp_path() ]),
  annotated_tree(Filename, Path).

-spec annotated_tree(binary(), [string()]) ->
   {ok, uri(), erlang_ls_parser:syntax_tree()} | {error, any()}.
annotated_tree(Filename, Path) ->
  case file:path_open(Path, Filename, [read]) of
    {ok, IoDevice, FullName} ->
      %% TODO: Avoid opening file twice
      file:close(IoDevice),
      {ok, Tree} = erlang_ls_parser:parse_file(FullName),
      Uri = erlang_ls_uri:uri(FullName),
      {ok, Uri, erlang_ls_parser:annotate(Tree)};
    {error, Error} ->
      {error, Error}
  end.

-spec otp_path() -> [string()].
otp_path() ->
  filelib:wildcard(filename:join([?OTP_INCLUDE_PATH, "*/src"])).

-spec app_path() -> [string()].
app_path() ->
  [ filename:join([?TEST_APP_PATH, "src"])
  , filename:join([?TEST_APP_PATH, "include"])
  , filename:join([?ERLANG_LS_PATH, "src"])
  , filename:join([?TEST_APP_PATH, "include"])
  ].

-spec deps_path() -> [string()].
deps_path() ->
  filelib:wildcard(filename:join([?DEPS_PATH, "*/src"])).

%% Look for a definition recursively in a file and its includes.
-spec search(binary(), [string()], any()) -> null | map().
search(Filename, Path, Thing) ->
  case annotated_tree(Filename, Path) of
    {ok, Uri, AnnotatedTree} ->
      case find(Uri, AnnotatedTree, Thing) of
        null ->
          Includes = erlang_ls_parser:find_poi_by_info_key(AnnotatedTree, include),
          IncludeLibs = erlang_ls_parser:find_poi_by_info_key(AnnotatedTree, include_lib),
          search_in_includes(Includes ++ IncludeLibs, Thing);
        Def ->
          Def
      end;
    {error, _Error} ->
      null
  end.

%% Look for a definition in a given tree
-spec find(uri(), erlang_ls_parser:syntax_tree(), any()) -> null | map().
find(Uri, AnnotatedTree, Thing) ->
  case erlang_ls_parser:find_poi_by_info(AnnotatedTree, Thing) of
    [#{ range := Range }|_] ->
      #{ uri => Uri, range => erlang_ls_protocol:range(Range) };
    [] ->
      null
  end.

-spec search_in_includes([erlang_ls_parser:poi()], string()) -> null | map().
search_in_includes([], _Thing) ->
  null;
search_in_includes([#{info := {include, Include0}}|T], Thing) ->
  Include = string:trim(Include0, both, [$"]),
  case search(list_to_binary(Include), app_path(), Thing) of
    null ->
      search_in_includes(T, Thing);
    Def ->
      Def
  end;
search_in_includes([#{info := {include_lib, Include0}}|T], Thing) ->
  Include = lists:last(filename:split(string:trim(Include0, both, [$"]))),
  case search(list_to_binary(Include), app_path(), Thing) of
    null ->
      search_in_includes(T, Thing);
    Def ->
      Def
  end.
