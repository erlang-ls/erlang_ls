%%==============================================================================
%% A client for the Erlang Language Server
%%==============================================================================
-module(els_client).

-callback start_link(any())     -> {ok, pid()}.
-callback send(pid(), iolist()) -> ok.

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).
%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , code_change/3
        ]).

%%==============================================================================
%% Exports
%%==============================================================================
%% API
-export([ cancel_request/1
        , completion/5
        , definition/3
        , did_open/4
        , did_save/1
        , did_close/1
        , document_symbol/1
        , exit/0
        , hover/3
        , initialize/2
        , references/3
        , document_highlight/3
        , document_formatting/3
        , document_rangeformatting/3
        , document_ontypeformatting/4
        , folding_range/1
        , shutdown/0
        , start_link/2
        , stop/0
        , workspace_symbol/1
        ]).

-export([ handle_responses/1 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, { transport_cb         :: transport_cb()
               , transport_server     :: pid()
               , request_id       = 1 :: request_id()
               , pending          = []
               }).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()        :: #state{}.
-type init_options() :: [].
-type transport()    :: stdio | tcp.
-type transport_cb() :: els_stdio_client | els_tcp_client.
-type request_id()   :: pos_integer().

%%==============================================================================
%% API
%%==============================================================================
-spec cancel_request(request_id()) -> ok.
cancel_request(Id) ->
  gen_server:call(?SERVER, {cancel_request, Id}).

%% TODO: More accurate and consistent parameters list
-spec completion( uri()
                , non_neg_integer()
                , non_neg_integer()
                , integer()
                , binary()
                ) ->
  ok.
completion(Uri, Line, Char, TriggerKind, TriggerCharacter) ->
  Opts = {Uri, Line, Char, TriggerKind, TriggerCharacter},
  gen_server:call(?SERVER, {completion, Opts}).

-spec definition(uri(), non_neg_integer(), non_neg_integer()) -> ok.
definition(Uri, Line, Char) ->
  gen_server:call(?SERVER, {definition, {Uri, Line, Char}}).

-spec hover(uri(), non_neg_integer(), non_neg_integer()) -> ok.
hover(Uri, Line, Char) ->
  gen_server:call(?SERVER, {hover, {Uri, Line, Char}}).

-spec references(uri(), non_neg_integer(), non_neg_integer()) -> ok.
references(Uri, Line, Char) ->
  gen_server:call(?SERVER, {references, {Uri, Line, Char}}).

-spec document_highlight(uri(), non_neg_integer(), non_neg_integer()) -> ok.
document_highlight(Uri, Line, Char) ->
  gen_server:call(?SERVER, {document_highlight, {Uri, Line, Char}}).

-spec document_formatting(uri(), non_neg_integer(), boolean()) ->
  ok.
document_formatting(Uri, TabSize, InsertSpaces) ->
  gen_server:call(?SERVER, {document_formatting, {Uri, TabSize, InsertSpaces}}).

-spec document_rangeformatting(uri(), range(), formatting_options()) ->
  ok.
document_rangeformatting(Uri, Range, FormattingOptions) ->
  gen_server:call(?SERVER, {document_rangeformatting,
                            {Uri, Range, FormattingOptions}}).

-spec document_ontypeformatting(uri(), position(), string()
                               , formatting_options()) ->
  ok.
document_ontypeformatting(Uri, Position, Char, FormattingOptions) ->
  gen_server:call(?SERVER, {document_ontypeformatting,
                            {Uri, Position, Char, FormattingOptions}}).

-spec did_open(uri(), binary(), number(), binary()) -> ok.
did_open(Uri, LanguageId, Version, Text) ->
  gen_server:call(?SERVER, {did_open, {Uri, LanguageId, Version, Text}}).

-spec did_save(uri()) -> ok.
did_save(Uri) ->
  gen_server:call(?SERVER, {did_save, {Uri}}).

-spec did_close(uri()) -> ok.
did_close(Uri) ->
  gen_server:call(?SERVER, {did_close, {Uri}}).

-spec document_symbol(uri()) ->
  ok.
document_symbol(Uri) ->
  gen_server:call(?SERVER, {document_symbol, {Uri}}).

-spec folding_range(uri()) -> ok.
folding_range(Uri) ->
  gen_server:call(?SERVER, {folding_range, {Uri}}).

-spec initialize(uri(), init_options()) -> map().
initialize(RootUri, InitOptions) ->
  gen_server:call(?SERVER, {initialize, {RootUri, InitOptions}}).

-spec shutdown() -> map().
shutdown() ->
  gen_server:call(?SERVER, {shutdown}).

-spec exit() -> ok.
exit() ->
  gen_server:call(?SERVER, {exit}).

-spec start_link(transport(), any()) -> {ok, pid()}.
start_link(Transport, Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {Transport, Args}, []).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

-spec workspace_symbol(string()) ->
  ok.
workspace_symbol(Query) ->
  gen_server:call(?SERVER, {workspace_symbol, {Query}}).

-spec handle_responses([map()]) -> ok.
handle_responses(Responses) ->
  gen_server:cast(?SERVER, {handle_responses, Responses}).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({transport(), any()}) -> {ok, state()}.
init({Transport, Args}) ->
  Cb = transport_cb(Transport),
  {ok, Pid} = Cb:start_link(Args),
  State = #state{transport_cb = Cb, transport_server = Pid},
  {ok, State}.

-spec handle_call(any(), any(), state()) -> {reply, any(), state()}.
handle_call({Action, Opts}, _From, State) when Action =:= did_save
                                               orelse Action =:= did_close
                                               orelse Action =:= did_open ->
  #state{transport_cb = Cb, transport_server = Server} = State,
  Method = method_lookup(Action),
  Params = notification_params(Opts),
  Content = els_protocol:notification(Method, Params),
  Cb:send(Server, Content),
  {reply, ok, State};
handle_call({exit}, _From, State) ->
  #state{transport_cb = Cb, transport_server = Server} = State,
  RequestId = State#state.request_id,
  Method = <<"exit">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  Cb:send(Server, Content),
  {reply, ok, State};
handle_call({shutdown}, From, State) ->
  #state{transport_cb = Cb, transport_server = Server} = State,
  RequestId = State#state.request_id,
  Method = <<"shutdown">>,
  Params = #{},
  Content = els_protocol:request(RequestId, Method, Params),
  Cb:send(Server, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }};
handle_call({cancel_request, Id}, _From, State) ->
  #state{transport_cb = Cb, transport_server = Server} = State,
  Method = <<"$/cancelRequest">>,
  Params = #{id => Id},
  Content = els_protocol:notification(Method, Params),
  Cb:send(Server, Content),
  {reply, ok, State};
handle_call(Input = {Action, _}, From, State) ->
  #state{ transport_cb     = Cb
        , transport_server = Server
        , request_id       = RequestId
        } = State,
  Method = method_lookup(Action),
  Params = request_params(Input),
  Content = els_protocol:request(RequestId, Method, Params),
  Cb:send(Server, Content),
  {noreply, State#state{ request_id = RequestId + 1
                       , pending    = [{RequestId, From} | State#state.pending]
                       }}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({handle_responses, Responses}, State) ->
  #state{pending = Pending0} = State,
  Pending = do_handle_responses(Responses, Pending0),
  {noreply, State#state{pending = Pending}};
handle_cast(_Request, State) ->
  {noreply, State}.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec do_handle_responses([map()], [any()]) -> [any()].
do_handle_responses([], Pending) ->
  Pending;
do_handle_responses([Response|Responses], Pending) ->
  case maps:find(id, Response) of
    {ok, RequestId} ->
      lager:debug("[CLIENT] Handling Response [response=~p]", [Response]),
      case lists:keyfind(RequestId, 1, Pending) of
        {RequestId, From} ->
          gen_server:reply(From, Response),
          do_handle_responses( Responses
                          , lists:keydelete(RequestId, 1, Pending));
        false ->
          do_handle_responses(Responses, Pending)
      end;
    error ->
      lager:debug( "[CLIENT] Handling Notification [notification=~p]"
                 , [Response]),
      do_handle_responses(Responses, Pending)
  end.

-spec method_lookup(atom()) -> binary().
method_lookup(completion)               -> <<"textDocument/completion">>;
method_lookup(definition)               -> <<"textDocument/definition">>;
method_lookup(document_symbol)          -> <<"textDocument/documentSymbol">>;
method_lookup(references)               -> <<"textDocument/references">>;
method_lookup(document_highlight)       -> <<"textDocument/documentHighlight">>;
method_lookup(document_formatting)      -> <<"textDocument/formatting">>;
method_lookup(document_rangeformatting) -> <<"textDocument/rangeFormatting">>;
method_lookup(document_ontypeormatting) -> <<"textDocument/onTypeFormatting">>;
method_lookup(did_open)                 -> <<"textDocument/didOpen">>;
method_lookup(did_save)                 -> <<"textDocument/didSave">>;
method_lookup(did_close)                -> <<"textDocument/didClose">>;
method_lookup(hover)                    -> <<"textDocument/hover">>;
method_lookup(workspace_symbol)         -> <<"workspace/symbol">>;
method_lookup(folding_range)            -> <<"textDocument/foldingRange">>;
method_lookup(initialize)               -> <<"initialize">>.

-spec request_params(tuple()) -> any().
request_params({document_symbol, {Uri}}) ->
  TextDocument = #{ uri => Uri },
  #{ textDocument => TextDocument };
request_params({workspace_symbol, {Query}}) ->
  #{ query => Query };
request_params({ completion
               , {Uri, Line, Char, TriggerKind, TriggerCharacter}}) ->
  #{ textDocument => #{ uri => Uri }
   , position     => #{ line      => Line - 1
                      , character => Char - 1
                      }
   , context => #{ triggerKind      => TriggerKind
                 , triggerCharacter => TriggerCharacter
                 }
   };
request_params({initialize, {RootUri, InitOptions}}) ->
  ContentFormat = [ ?MARKDOWN , ?PLAINTEXT ],
  TextDocument = #{ <<"completion">> =>
                    #{ <<"contextSupport">> => 'true' }
                  , <<"hover">> =>
                    #{ <<"contentFormat">> => ContentFormat }
                  },
  #{ <<"rootUri">> => RootUri
   , <<"initializationOptions">> => InitOptions
   , <<"capabilities">> => #{ <<"textDocument">> => TextDocument }
   };
request_params({ document_formatting
               , {Uri, TabSize, InsertSpaces}}) ->
  #{ textDocument => #{ uri => Uri }
   , options      => #{ tabSize      => TabSize
                      , insertSpaces => InsertSpaces
                      }
   };
request_params({folding_range, {Uri}}) ->
  TextDocument = #{ uri => Uri },
  #{ textDocument => TextDocument };
request_params({_Action, {Uri, Line, Char}}) ->
  #{ textDocument => #{ uri => Uri }
   , position     => #{ line      => Line - 1
                      , character => Char - 1
                      }
   }.

-spec notification_params(tuple()) -> map().
notification_params({Uri}) ->
  TextDocument = #{ uri => Uri },
  #{textDocument => TextDocument};
notification_params({Uri, LanguageId, Version, Text}) ->
  TextDocument = #{ uri        => Uri
                  , languageId => LanguageId
                  , version    => Version
                  , text       => Text
                  },
  #{textDocument => TextDocument}.

-spec transport_cb(transport()) -> transport_cb().
transport_cb(stdio)             -> els_stdio_client;
transport_cb(tcp)               -> els_tcp_client.
