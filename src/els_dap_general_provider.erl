%%==============================================================================
%% @doc Erlang DAP General Provider
%%
%% Implements the logic for hanlding all of the commands in the protocol.
%%
%% The functionality in this module will eventually be broken into several
%% different providers.
%% @end
%%==============================================================================
-module(els_dap_general_provider).

-behaviour(els_provider).
-export([ handle_request/2
        , handle_info/2
        , is_enabled/0
        , init/0
        ]).

-export([ capabilities/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

%% Protocol
-type capabilities() :: #{}.
-type request()      :: {Command :: binary(), Params :: map()}.
-type result()       :: #{}.

%% Internal
-type frame_id()     :: pos_integer().
-type frame()        :: #{ module    := module()
                         , function  := atom()
                         , arguments := [any()]
                         , source    := binary()
                         , line      := integer()
                         , bindings  := any()}.
-type thread()       :: #{ pid := pid()
                         , frames := #{frame_id() => frame()}
                         }.
-type thread_id()    :: integer().
-type state()        :: #{ threads => #{thread_id() => thread()}
                         , project_node => atom()
                         , launch_params => #{}
                         , scope_bindings =>
                          #{pos_integer() => {binding_type(), bindings()}}
                         }.
-type bindings()     :: [{varname(), term()}].
-type varname()      :: atom() | string().
%% extendable bindings type for customized pretty printing
-type binding_type() :: generic | map_assoc.
%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec init() -> state().
init() ->
  #{ threads => #{}
   , launch_params => #{} }.

-spec handle_request(request(), state()) -> {result(), state()}.
handle_request({<<"initialize">>, _Params}, State) ->
  %% quick fix to satisfy els_config initialization
  {ok, RootPath} = file:get_cwd(),
  RootUri = els_uri:uri(els_utils:to_binary(RootPath)),
  InitOptions = #{},
  ok = els_config:initialize(RootUri, capabilities(), InitOptions),
  {capabilities(), State};
handle_request({<<"launch">>, Params}, State) ->
  #{<<"cwd">> := Cwd} = Params,
  ok = file:set_cwd(Cwd),
  Name = filename:basename(Cwd),
  ProjectNode =
    case Params of
      #{ <<"projectnode">> := Node } -> binary_to_atom(Node, utf8);
      _ -> els_distribution_server:node_name(<<"erlang_ls_dap_project">>, Name)
    end,
  case Params of
    #{ <<"runinterminal">> := Cmd
     } ->
      ParamsR
        = #{ <<"kind">> => <<"integrated">>
           , <<"title">> => ProjectNode
           , <<"cwd">> => Cwd
           , <<"args">> => Cmd
           },
      ?LOG_INFO("Sending runinterminal request: [~p]", [ParamsR]),
      els_dap_server:send_request(<<"runInTerminal">>, ParamsR),
      ok;
    _ ->
      ?LOG_INFO("launching 'rebar3 shell`", []),
      spawn(fun() ->
                els_utils:cmd("rebar3", ["shell", "--name", ProjectNode]) end)
  end,
  LocalNode = els_distribution_server:node_name(<<"erlang_ls_dap">>, Name),
  els_distribution_server:start_distribution(LocalNode),
  ?LOG_INFO("Distribution up on: [~p]", [LocalNode]),

  els_dap_server:send_event(<<"initialized">>, #{}),

  {#{}, State#{project_node => ProjectNode, launch_params => Params}};
handle_request( {<<"configurationDone">>, _Params}
              , #{ project_node := ProjectNode
                 , launch_params := LaunchParams} = State
              ) ->
  ?LOG_INFO("Connecting to: [~p]", [ProjectNode]),
  els_distribution_server:wait_connect_and_monitor(ProjectNode),

  inject_dap_agent(ProjectNode),

  %% TODO: Fetch stack_trace mode from Launch Config
  els_dap_rpc:stack_trace(ProjectNode, all),
  MFA = {els_dap_agent, int_cb, [self()]},
  els_dap_rpc:auto_attach(ProjectNode, [break], MFA),

  case LaunchParams of
    #{ <<"module">> := Module
     , <<"function">> := Function
     , <<"args">> := Args
     } ->
      M = binary_to_atom(Module, utf8),
      F = binary_to_atom(Function, utf8),
      A = els_dap_rpc:eval(ProjectNode, Args, []),
      ?LOG_INFO("Launching MFA: [~p]", [{M, F, A}]),
      rpc:cast(ProjectNode, M, F, A);
    _ -> ok
  end,
  {#{}, State};
handle_request( {<<"setBreakpoints">>, Params}
              , #{project_node := ProjectNode} = State
              ) ->
  #{<<"source">> := #{<<"path">> := Path}} = Params,
  SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
  _SourceModified = maps:get(<<"sourceModified">>, Params, false),
  Module = els_uri:module(els_uri:uri(Path)),

  %% AZ: we should have something like `ensure_connected`
  ?LOG_INFO("Connecting to: [~p]", [ProjectNode]),
  els_distribution_server:wait_connect_and_monitor(ProjectNode),

  %% TODO: Keep a list of interpreted modules, not to re-interpret them
  els_dap_rpc:i(ProjectNode, Module),
  [els_dap_rpc:break(ProjectNode, Module, Line) ||
    #{<<"line">> := Line} <- SourceBreakpoints],
  Breakpoints = [#{<<"verified">> => true, <<"line">> => Line} ||
                  #{<<"line">> := Line} <- SourceBreakpoints],
  {#{<<"breakpoints">> => Breakpoints}, State};
handle_request({<<"setExceptionBreakpoints">>, _Params}, State) ->
  {#{}, State};
handle_request({<<"setFunctionBreakpoints">>, Params}
              , #{project_node := ProjectNode} = State
              ) ->
  FunctionBreakPoints = maps:get(<<"breakpoints">>, Params, []),
  els_distribution_server:wait_connect_and_monitor(ProjectNode),
  MFAs = [
    begin
      Spec = {Mod, _, _} = parse_mfa(MFA),
      els_dap_rpc:i(ProjectNode, Mod),
      Spec
      end
       ||
    #{<<"name">> := MFA, <<"enabled">>:= Enabled} <- FunctionBreakPoints,
    Enabled andalso parse_mfa(MFA) =/= error
  ],
  [ els_dap_rpc:break_in(ProjectNode, Mod, Func, Arity)
    || {Mod, Func, Arity} <- MFAs
  ],
  Breakpoints =
    [ #{ <<"verified">> => true
      ,  <<"line">>     => Line
      , <<"source">>    => #{ <<"path">> => source(Module, ProjectNode)}}
    || {{Module, Line}, [Status, _, _, _]}
        <- els_dap_rpc:all_breaks(ProjectNode)
    , Status =:= active],
  {#{<<"breakpoints">> => Breakpoints}, State};
handle_request({<<"threads">>, _Params}, #{threads := Threads0} = State) ->
  Threads =
    [ #{ <<"id">> => Id
       , <<"name">> => els_utils:to_binary(io_lib:format("~p", [Pid]))
       } || {Id, #{pid := Pid} = _Thread} <- maps:to_list(Threads0)
    ],
  {#{<<"threads">> => Threads}, State};
handle_request({<<"stackTrace">>, Params}, #{threads := Threads} = State) ->
  #{<<"threadId">> := ThreadId} = Params,
  Thread = maps:get(ThreadId, Threads),
  Frames = maps:get(frames, Thread),
  StackFrames =
    [ #{ <<"id">> => Id
       , <<"name">> => format_mfa(M, F, length(A))
       , <<"source">> => #{<<"path">> => Source}
       , <<"line">> => Line
       , <<"column">> => 0
       }
      || { Id
         , #{ module := M
            , function := F
            , arguments := A
            , line := Line
            , source := Source
            }
         } <- maps:to_list(Frames)
    ],
  {#{<<"stackFrames">> => StackFrames}, State};
handle_request({<<"scopes">>, #{<<"frameId">> := FrameId} }
                 , #{ threads := Threads
                 } = State) ->
  Frame = frame_by_id(FrameId, maps:values(Threads)),
  Bindings = maps:get(bindings, Frame),
  Ref = erlang:unique_integer([positive]),
  {#{<<"scopes">> => [
    #{
      <<"name">> => <<"Locals">>,
      <<"presentationHint">> => <<"locals">>,
      <<"variablesReference">> => Ref
    }
  ]}, State#{scope_bindings => #{Ref => {generic, Bindings}}}};
handle_request( {<<"next">>, Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
              ) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = to_pid(ThreadId, Threads),
  ok = els_dap_rpc:next(ProjectNode, Pid),
  {#{}, State};
handle_request( {<<"continue">>, Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
              ) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = to_pid(ThreadId, Threads),
  ok = els_dap_rpc:continue(ProjectNode, Pid),
  {#{}, State};
handle_request( {<<"stepIn">>, Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
              ) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = to_pid(ThreadId, Threads),
  ok = els_dap_rpc:step(ProjectNode, Pid),
  {#{}, State};
handle_request( {<<"stepOut">>, Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
              ) ->
  #{<<"threadId">> := ThreadId} = Params,
  Pid = to_pid(ThreadId, Threads),
  ok = els_dap_rpc:next(ProjectNode, Pid),
  {#{}, State};
handle_request({<<"evaluate">>, #{ <<"context">> := <<"hover">>
                                 , <<"frameId">> := FrameId
                                 , <<"expression">> := Input
                                 } = _Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
              ) ->
  Frame = frame_by_id(FrameId, maps:values(Threads)),
  Bindings = maps:get(bindings, Frame),
  Return = els_dap_rpc:eval(ProjectNode, Input, Bindings),
  Result = els_utils:to_binary(io_lib:format("~p", [Return])),
  {#{<<"result">> => Result}, State};
handle_request({<<"variables">>, #{<<"variablesReference">> := Ref
                                  } = _Params}
              , #{ scope_bindings := AllBindings
                 } = State) ->
  #{Ref := {Type, Bindings}} = AllBindings,
  {Variables, MoreBindings} = build_variables(Type, Bindings),
  { #{<<"variables">> => Variables}
  , State#{ scope_bindings => maps:merge(AllBindings, MoreBindings)}};
handle_request({<<"disconnect">>, _Params}, State) ->
  els_utils:halt(0),
  {#{}, State}.

-spec handle_info(any(), state()) -> state().
handle_info( {int_cb, ThreadPid}
           , #{ threads := Threads
              , project_node := ProjectNode
              } = State
           ) ->
  ?LOG_DEBUG("Int CB called. thread=~p", [ThreadPid]),
  ThreadId = id(ThreadPid),
  Thread = #{ pid    => ThreadPid
            , frames => stack_frames(ThreadPid, ProjectNode)
            },
  els_dap_server:send_event(<<"stopped">>, #{ <<"reason">> => <<"breakpoint">>
                                            , <<"threadId">> => ThreadId
                                            }),
  State#{threads => maps:put(ThreadId, Thread, Threads)}.

%%==============================================================================
%% API
%%==============================================================================

-spec capabilities() -> capabilities().
capabilities() ->
  #{ <<"supportsConfigurationDoneRequest">> => true
   , <<"supportsEvaluateForHovers">> => true
   , <<"supportsFunctionBreakpoints">> => true}.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec inject_dap_agent(atom()) -> ok.
inject_dap_agent(Node) ->
  Module = els_dap_agent,
  {Module, Bin, File} = code:get_object_code(Module),
  {_Replies, _} = els_dap_rpc:load_binary(Node, Module, File, Bin),
  ok.

-spec id(pid()) -> integer().
id(Pid) ->
  erlang:phash2(Pid).

-spec stack_frames(pid(), atom()) -> #{frame_id() => frame()}.
stack_frames(Pid, Node) ->
  {ok, Meta} = els_dap_rpc:get_meta(Node, Pid),
  %% TODO: Also examine rest of list
  [{_Level, {M, F, A}} | _] =
    els_dap_rpc:meta(Node, Meta, backtrace, all),
  Bindings = els_dap_rpc:meta(Node, Meta, bindings, nostack),
  StackFrameId = erlang:unique_integer([positive]),
  StackFrame = #{ module    => M
                , function  => F
                , arguments => A
                , source    => source(M, Node)
                , line      => break_line(Pid, Node)
                , bindings  => Bindings
                },
  #{StackFrameId => StackFrame}.

-spec break_line(pid(), atom()) -> integer().
break_line(Pid, Node) ->
  Snapshots = els_dap_rpc:snapshot(Node),
  {Pid, _Function, break, {_Module, Line}} = lists:keyfind(Pid, 1, Snapshots),
  Line.

-spec source(atom(), atom()) -> binary().
source(Module, Node) ->
  CompileOpts = els_dap_rpc:module_info(Node, Module, compile),
  Source = proplists:get_value(source, CompileOpts),
  unicode:characters_to_binary(Source).

-spec to_pid(pos_integer(), #{thread_id() => thread()}) -> pid().
to_pid(ThreadId, Threads) ->
  Thread = maps:get(ThreadId, Threads),
  maps:get(pid, Thread).

-spec frame_by_id(frame_id(), [thread()]) -> frame().
frame_by_id(FrameId, Threads) ->
  [Frame] = [ maps:get(FrameId, Frames)
              ||  #{frames := Frames} <- Threads, maps:is_key(FrameId, Frames)
            ],
  Frame.

-spec format_mfa(module(), atom(), integer()) -> binary().
format_mfa(M, F, A) ->
  els_utils:to_binary(io_lib:format("~p:~p/~p", [M, F, A])).

-spec parse_mfa(string()) -> {module(), atom(), non_neg_integer()} | error.
parse_mfa(MFABinary) ->
  MFA = unicode:characters_to_list(MFABinary),
  case erl_scan:string(MFA) of
    {ok, [ {'fun', _}
         , {atom, _, Module}
         , {':', _}
         , {atom, _, Function}
         , {'/', _}
         , {integer, _, Arity}], _} when Arity >= 0 ->
      {Module, Function, Arity};
    {ok, [ {atom, _, Module}
         , {':', _}
         , {atom, _, Function}
         , {'/', _}
         , {integer, _, Arity}], _} when Arity >= 0 ->
      {Module, Function, Arity};
    _ ->
      error
  end.

-spec build_variables(binding_type(), bindings()) ->
  {[any()], #{pos_integer() => bindings()}}.
build_variables(Type, Bindings) ->
  build_variables(Type, Bindings, {[], #{}}).

-spec build_variables(binding_type(), bindings(), Acc) -> Acc
  when Acc :: {[any()], #{pos_integer() => bindings()}}.
build_variables(_, [], Acc) ->
  Acc;
build_variables(generic, [{Name, Value} | Rest], Acc) when is_list(Value) ->
  build_variables(
    generic,
    Rest,
    add_var_to_acc(Name, Value, build_list_bindings(Value), Acc)
  );
build_variables(generic, [{Name, Value} | Rest], Acc) when is_tuple(Value) ->
  build_variables(
    generic,
    Rest,
    add_var_to_acc(Name, Value, build_tuple_bindings(Value), Acc)
  );
build_variables(generic, [{Name, Value} | Rest], Acc) when is_map(Value) ->
  build_variables(
    generic,
    Rest,
    add_var_to_acc(Name, Value, build_map_bindings(Value), Acc)
  );
build_variables(generic, [{Name, Value} | Rest], Acc) ->
  build_variables(
    generic,
    Rest,
    add_var_to_acc(Name, Value, none, Acc)
  );
build_variables(map_assoc, [{Name, Assocs} | Rest], Acc) ->
  {_, [{'Value', Value}, {'Key', Key}]} = Assocs,
  build_variables(
    map_assoc,
    Rest,
    add_var_to_acc(Name, {Key, Value}, Assocs, Acc)
  ).

-spec add_var_to_acc(
  varname(),
  term(),
  none | {binding_type(), bindings()},
  Acc
) -> Acc
  when Acc :: {[any()], #{non_neg_integer() => bindings()}}.
add_var_to_acc(Name, Value, none, {VarAcc, BindAcc}) ->
  { [build_variable(Name, Value, 0) | VarAcc]
  , BindAcc};
add_var_to_acc(Name, Value, Bindings, {VarAcc, BindAcc}) ->
  Ref = erlang:unique_integer([positive]),
  { [build_variable(Name, Value, Ref) | VarAcc]
  , BindAcc#{ Ref => Bindings}
  }.

-spec build_variable(varname(), term(), non_neg_integer()) -> any().
build_variable(Name, Value, Ref) ->
  %% print whole term to enable copying if the value
  #{ <<"name">> => unicode:characters_to_binary(io_lib:format("~s", [Name]))
   , <<"value">> => unicode:characters_to_binary(io_lib:format("~p", [Value]))
   , <<"variablesReference">> => Ref }.

-spec build_list_bindings(
  maybe_improper_list()
) -> {binding_type(), bindings()}.
build_list_bindings(List) ->
  build_maybe_improper_list_bindings(List, 0, []).

-spec build_tuple_bindings(tuple()) -> {binding_type(), bindings()}.
build_tuple_bindings(Tuple) ->
  build_list_bindings(erlang:tuple_to_list(Tuple)).

-spec build_map_bindings(map()) -> {binding_type(), bindings()}.
build_map_bindings(Map) ->
  {_, Bindings} =
    lists:foldl(
      fun ({Key, Value}, {Cnt, Acc}) ->
        Name =
          unicode:characters_to_binary(
            io_lib:format("~p => ~p", [Key, Value])),
        { Cnt + 1
        , [{ Name
           , {generic, [{'Value', Value}, {'Key', Key}]}
           } | Acc]
        }
      end,
      {0, []}, maps:to_list(Map)),
  {map_assoc, Bindings}.

-spec build_maybe_improper_list_bindings(
  maybe_improper_list(),
  non_neg_integer(),
  bindings()
) -> {binding_type(), bindings()}.
build_maybe_improper_list_bindings([], _, Acc) ->
  {generic, Acc};
build_maybe_improper_list_bindings([E | Tail], Cnt, Acc) ->
  Binding = {erlang:integer_to_list(Cnt), E},
  build_maybe_improper_list_bindings(Tail, Cnt + 1, [Binding | Acc]);
build_maybe_improper_list_bindings(ImproperTail, _Cnt, Acc) ->
  Binding = {"improper tail", ImproperTail},
  build_maybe_improper_list_bindings([], 0, [Binding | Acc]).
