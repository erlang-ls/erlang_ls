%%==============================================================================
%% Buffer gen_server
%%==============================================================================
-module(erlang_ls_buffer).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ start_link/1
        , set_text/2
        , get_completions/3
        , get_mfa/3
        , get_element_at_pos/3
        ]).

%% gen_server callbacks
-export([ code_change/3
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , init/1
        , terminate/2
        ]).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, {text}).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state() :: #state{}.

%%%===================================================================
%%% API
%%%===================================================================
-spec start_link(binary()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Text) ->
  gen_server:start_link(?MODULE, [Text], []).

-spec set_text(pid(), binary()) -> ok.
set_text(Pid, Text) ->
  gen_server:call(Pid, {set_text, Text}).

-spec get_completions(pid(), non_neg_integer(), non_neg_integer()) ->
  [{module(), binary()}].
get_completions(Pid, Line, Char) ->
  gen_server:call(Pid, {get_completions, Line, Char}).

-spec get_mfa(pid(), non_neg_integer(), non_neg_integer()) ->
  {module(), atom(), non_neg_integer()}.
get_mfa(Pid, Line, Char) ->
  gen_server:call(Pid, {get_mfa, Line, Char}).

-spec get_element_at_pos(pid(), non_neg_integer(), non_neg_integer()) ->
  [any()].
get_element_at_pos(Pid, Line, Char) ->
  gen_server:call(Pid, {get_element_at_pos, Line, Char}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-spec init([binary()]) -> {ok, state()}.
init([Text]) ->
  process_flag(trap_exit, true),
  {ok, #state{ text = Text}}.

-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call({set_text, Text}, _From, State) ->
  Reply = ok,
  {reply, Reply, State#state{text = Text}};
handle_call({get_completions, Line, Char}, _From, #state{ text = Text
                                                        } = State) ->
  Reply = do_get_completions(Text, Line, Char),
  {reply, Reply, State};
handle_call({get_mfa, Line, Char}, _From, #state{ text = Text
                                                } = State) ->
  Reply = do_get_mfa(Text, Line, Char),
  {reply, Reply, State};
handle_call({get_element_at_pos, Line, Char}, _From, #state{ text = Text
                                                           } = State) ->
  Reply = do_get_element_at_pos(Text, Line, Char),
  {reply, Reply, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(any(), state()) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(any(), state(), any()) -> {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec do_get_completions(binary(), non_neg_integer(), pos_integer()) ->
  [map()].
do_get_completions(Text, Line, Character) ->
  LineText        = get_line_text(Text, Line),
  LineBeforeChar  = binary:part(LineText, {0, Character - 1}),
  Trigger         = binary:part(LineText, {Character - 1, 1}),
  %% TODO: Can't we get the context from the client?
  case Trigger of
    <<":">> ->
      {ok, Tokens, _} = erl_scan:string(binary_to_list(LineBeforeChar)),
      [H| _] = lists:reverse(Tokens),
      case H of
        {atom, _, Module} ->
          do_get_completions(Module)
      end;
    <<"#">> ->
      [#{label => list_to_binary(io_lib:format("~p", [RD]))} ||
        RD <- erlang_ls_completion:record_definitions()];
    _ ->
      []
  end.

-spec do_get_mfa(binary(), integer(), integer()) ->
  {module(), atom(), non_neg_integer()}.
do_get_mfa(Text, Line, _Character) ->
  LineText        = get_line_text(Text, Line),
  {ok, Tokens, _} = erl_scan:string(binary_to_list(LineText)),
  [{atom, _, M}, {':', _}, {atom, _, F}, {'/', _}, {integer, _, A}] = Tokens,
  {M, F, A}.

-spec do_get_element_at_pos(binary(), non_neg_integer(), non_neg_integer()) ->
   [erlang_ls_poi:poi()].
do_get_element_at_pos(Text, Line, Column) ->
  %% TODO: Cache tree
  {ok, Tree} = erlang_ls_parser:parse(Text),
  AnnotatedTree = erlang_ls_tree:annotate(Tree),
  erlang_ls_parser:find_poi_by_pos(AnnotatedTree, {Line, Column}).

-spec get_line_text(binary(), integer()) -> binary().
get_line_text(Text, Line) ->
  Lines = binary:split(Text, <<"\n">>, [global]),
  lists:nth(Line + 1, Lines).

-spec function_name_to_binary(atom(), non_neg_integer()) -> binary().
function_name_to_binary(Function, Arity) ->
  list_to_binary(io_lib:format("~p/~p", [Function, Arity])).

-spec do_get_completions(module()) -> [map()].
do_get_completions(Module) ->
  try Module:module_info(exports) of
      Info ->
      CS = [{Module, function_name_to_binary(F, A)} || {F, A} <- Info],
      [#{ label => C
        , data => M
        , documentation => erlang_ls_doc:get_doc(M, C)
        } || {M, C} <- CS]
  catch _:_ ->
      []
  end.
