%%==============================================================================
%% The Buffer Server
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
        , index/1
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

-spec index(uri()) -> ok.
index(Uri) ->
  gen_server:call(?SERVER, {index, Uri}).

-spec stop() -> ok.
stop() ->
  gen_server:stop(?SERVER).

%%==============================================================================
%% gen_server Callback Functions
%%==============================================================================
-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #state{}}.

-spec handle_call(any(), any(), state()) -> {reply, ok, state()}.
handle_call({index, Uri}, _From, State) ->
  ok = do_index(Uri),
  {reply, ok, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec do_index(uri()) -> ok.
do_index(Uri) ->
  {ok, IoDevice} = file:open(Uri, [read]),
  {ok, Forms} = epp_dodger:parse(IoDevice, {1, 1}),
  erl_syntax_lib:mapfold(fun analyze_form/2, #{ module => undefined
                                              , macros => []
                                              , variables => []
                                              , applications => []
                                              }, erl_syntax:form_list(Forms)),
  ok = file:close(IoDevice),
  ok.

-spec analyze_form(erl_syntax:syntax_tree(), map()) -> map().
analyze_form(Form, Acc) ->
  NewAcc = try erl_syntax_lib:analyze_form(Form) of
               Type -> analyze_form(Form, Type, Acc)
           catch _:_ ->
               T = erl_syntax:type(Form),
               erlang:display(T),
               analyze_tree(Form, T, Acc)
           end,
  {Form, NewAcc}.

analyze_form(Form, {attribute, preprocessor}, Acc) ->
  Name = erl_syntax:attribute_name(Form),
  [Var|_] = erl_syntax:attribute_arguments(Form),
  case erl_syntax:atom_name(Name) of
    "define" ->
      Macros = maps:get(macros, Acc),
      Pos = erl_syntax:get_pos(Form),
      Macro = #{ pos => Pos
               , name => erl_syntax:variable_name(Var)
               },
      maps:put(macros, [Macro|Macros], Acc);
    _ ->
      Acc
  end;
analyze_form(_Form, {attribute, {module, {Module, _Variables}}}, Acc) ->
  maps:put(module, Module, Acc);
analyze_form(_Form, {attribute, {module, Module}}, Acc) ->
  maps:put(module, Module, Acc);
analyze_form(_Form, _, Acc) ->
  Acc.

analyze_tree(Form, application, Acc) ->
  Pos = erl_syntax:get_pos(Form),
  Application = case erl_syntax_lib:analyze_application(Form) of
                  {M, {F, A}} ->
                    #{ pos => Pos
                     , name => {M, F, A}
                     };
                  {F, A} ->
                    #{ pos => Pos
                     , name => {maps:get(module, Acc), F, A}
                     }
                end,
  maps:put(applications, [Application | maps:get(applications, Acc)], Acc);
analyze_tree(Form, variable, Acc) ->
  Variable = #{ pos => erl_syntax:get_pos(Form)
              , name => erl_syntax:variable_name(Form)
              },
  maps:put(variables, [Variable | maps:get(variables, Acc)], Acc);
analyze_tree(_Form, _Type, Acc) ->
  Acc.

%% TODO: Introduce ETS and avoid fold
%% TODO: Rename into analyzer
%% TODO: Analyze on init
%% TODO: Assume OTP structure on init
%% TODO: Function to append to map field
%% TODO: We should input binaries, not files. Could we patch epp?
%% TODO: Avoid epp parsing twice
