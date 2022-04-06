%%%=============================================================================
%%% @doc Buffer edits to an open buffer to avoid re-indexing too often.
%%% @end
%%%=============================================================================
-module(els_buffer_server).

%%==============================================================================
%% API
%%==============================================================================
-export([ new/2
        , stop/1
        , apply_edits/2
        , flush/1
        ]).

-export([ start_link/2 ]).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-behaviour(gen_server).
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(FLUSH_DELAY, 200). %% ms

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type text() :: binary().
-type state() :: #{ uri := uri()
                  , text := text()
                  , ref := undefined | reference()
                  , pending := [{pid(), any()}]
                  }.
-type buffer() :: pid().

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").
-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec new(uri(), text()) -> {ok, pid()}.
new(Uri, Text) ->
  supervisor:start_child(els_buffer_sup, [Uri, Text]).

-spec stop(buffer()) -> ok.
stop(Buffer) ->
  supervisor:terminate_child(els_buffer_sup, Buffer).

-spec apply_edits(buffer(), [els_text:edit()]) -> ok.
apply_edits(Buffer, Edits) ->
  gen_server:cast(Buffer, {apply_edits, Edits}).

-spec flush(buffer()) -> text().
flush(Buffer) ->
  gen_server:call(Buffer, {flush}).

-spec start_link(uri(), text()) -> {ok, buffer()}.
start_link(Uri, Text) ->
  gen_server:start_link(?MODULE, {Uri, Text}, []).

%%==============================================================================
%% Callbacks for the gen_server behaviour
%%==============================================================================
-spec init({uri(), text()}) -> {ok, state()}.
init({Uri, Text}) ->
  schedule_flush(),
  {ok, #{ uri => Uri, text => Text, ref => undefined, pending => [] }}.

-spec handle_call(any(), {pid(), any()}, state()) -> {reply, any(), state()}.
handle_call({flush}, From, State) ->
  #{uri := Uri, ref := Ref0, pending := Pending0} = State,
  ?LOG_INFO("[~p] Flushing request [uri=~p]", [?MODULE, Uri]),
  cancel_flush(Ref0),
  Ref = schedule_flush(),
  {noreply, State#{ref => Ref, pending => [From|Pending0]}};
handle_call(Request, _From, State) ->
  {reply, {not_implemented, Request}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast({apply_edits, Edits}, #{uri := Uri} = State) ->
  ?LOG_INFO("[~p] Applying edits [uri=~p]", [?MODULE, Uri]),
  #{text := Text0, ref := Ref0} = State,
  cancel_flush(Ref0),
  Text = els_text:apply_edits(Text0, Edits),
  Ref = schedule_flush(),
  {noreply, State#{text => Text, ref => Ref}}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(flush, #{uri := Uri, text := Text, pending := Pending0} = State) ->
  ?LOG_INFO("[~p] Flushing [uri=~p]", [?MODULE, Uri]),
  do_flush(Uri, Text),
  [gen_server:reply(From, Text) || From <- Pending0],
  {noreply, State#{pending => []}};
handle_info(_Request, State) ->
  {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec schedule_flush() -> reference().
schedule_flush() ->
  erlang:send_after(?FLUSH_DELAY, self(), flush).

-spec cancel_flush(undefined | reference()) -> ok.
cancel_flush(undefined) ->
  ok;
cancel_flush(Ref) ->
  erlang:cancel_timer(Ref),
  ok.

-spec do_flush(uri(), text()) -> ok.
do_flush(Uri, Text) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  els_indexing:deep_index(Document#{text => Text, buffer => self()}).
