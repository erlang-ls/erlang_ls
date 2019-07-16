%%==============================================================================
%% The Indexer
%%==============================================================================
-module(erlang_ls_indexer).

%%==============================================================================
%% Behaviours
%%==============================================================================
-behaviour(gen_server).

%%==============================================================================
%% Exports
%%==============================================================================
%% API
-export([ start_link/0
        , index/2
        , stop/0
        ]).

%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% Defines
%%==============================================================================
-define(SERVER, ?MODULE).
-define(TABLE,  ?MODULE).

%%==============================================================================
%% Record Definitions
%%==============================================================================
-record(state, {}).

%%==============================================================================
%% Type Definitions
%%==============================================================================
-type state()  :: #state{}.

%%%=============================================================================
%%% API
%%%=============================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, {}, []).

-spec index(uri(), binary()) -> ok.
index(Uri, Text) ->
  gen_server:call(?SERVER, {index, Uri, Text}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({}) -> {ok, state()}.
init({}) ->
  ets:new(?TABLE, [named_table, bag, protected, {read_concurrency, true}]),
  {ok, #state{}}.

-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call({index, Uri, Text}, _From, State) ->
  ok = do_index(Uri, Text),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec do_index(uri(), binary()) -> ok.
do_index(Uri, Text) ->
  %% TODO: Avoid writing
  Path = "/tmp/erlang_ls_tmp",
  ok = file:write_file(Path, Text),
  {ok, IoDevice} = file:open(Path, [read]),
  {ok, Forms} = epp_dodger:parse(IoDevice, {1, 1}),
  F = fun(Form) -> index_form(Form, Uri) end,
  erl_syntax_lib:map(F, erl_syntax:form_list(Forms)),
  ok = file:close(IoDevice),
  ok.

-spec index_form(erl_syntax:syntax_tree(), uri()) -> erl_syntax:syntax_tree().
index_form(Form, Uri) ->
  Pos = erl_syntax:get_pos(Form),
  try erl_syntax_lib:analyze_form(Form) of
      {attribute, Info} ->
        index_attribute(Form, Info, Uri, Pos);
      _ ->
        ok
  catch _:_ ->
      Type = erl_syntax:type(Form),
      index_node(Form, Type, Uri, Pos)
  end,
  Form.

index_attribute(_Form, {module, {Module, _Variables}}, Uri, Pos) ->
  ets:insert(?TABLE, {{Uri, module}, Module, Pos});
index_attribute(_Form, {module, Module}, Uri, Pos) ->
  ets:insert(?TABLE, {{Uri, module}, Module, Pos});
index_attribute(Form, preprocessor, Uri, Pos) ->
  AttributeName = erl_syntax:atom_name(erl_syntax:attribute_name(Form)),
  [Variable|_] = erl_syntax:attribute_arguments(Form),
  case AttributeName of
    "define" ->
      VariableName = erl_syntax:variable_name(Variable),
      ets:insert(?TABLE, {{Uri, macro}, {VariableName, Pos}});
    _ ->
      ok
  end;
index_attribute(_Form, _Info, _Uri, _Pos) ->
  ok.

index_node(Form, application, Uri, Pos) ->
  Application = case erl_syntax_lib:analyze_application(Form) of
                  {M, {F, A}} ->
                    {M, F, A};
                  {F, A} ->
                    [{{Uri, module}, M, _Pos}] = ets:lookup(?TABLE, {Uri, module}),
                    {M, F, A}
                end,
  ets:insert(?TABLE, {{Uri, application}, Application, Pos});
index_node(Form, variable, Uri, Pos) ->
  VariableName = erl_syntax:variable_name(Form),
  ets:insert(?TABLE, {{Uri, variable}, VariableName, Pos});
index_node(_Form, _Type, _Uri, _Pos) ->
  ok.
