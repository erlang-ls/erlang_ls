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
                         , line      := line()
                         , bindings  := any()
                         }.
-type thread()       :: #{ pid := pid()
                         , frames := #{frame_id() => frame()}
                         }.
-type thread_id()    :: integer().
-type state()        :: #{ threads => #{thread_id() => thread()}
                         , project_node => atom()
                         , launch_params => #{}
                         , scope_bindings =>
                          #{pos_integer() => {binding_type(), bindings()}}
                         , breakpoints := breakpoints()
                         }.
-type bindings()     :: [{varname(), term()}].
-type varname()      :: atom() | string().
%% extendable bindings type for customized pretty printing
-type binding_type() :: generic | map_assoc.
-type breakpoints() :: #{module() => #{lines => [line()], function => [function_break()]}}.
-type line()         :: non_neg_integer().
-type function_break() :: {atom(), non_neg_integer()}.
%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec init() -> state().
init() ->
  #{ threads => #{}
   , launch_params => #{}
   , scope_bindings => #{}
   , breakpoints => #{}}.

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

  %% start distribution
  LocalNode = els_distribution_server:node_name(<<"erlang_ls_dap">>, Name),
  els_distribution_server:start_distribution(LocalNode),
  ?LOG_INFO("Distribution up on: [~p]", [LocalNode]),

  %% get configuration for project node and cookie
  ProjectNode =
    case Params of
      #{ <<"projectnode">> := ConfNode } -> binary_to_atom(ConfNode, utf8);
      _ -> els_distribution_server:node_name(<<"erlang_ls_dap_project">>, Name)
    end,
  Cookie =
     case Params of
      #{ <<"cookie">> := ConfCookie } -> binary_to_atom(ConfCookie, utf8);
      _ -> erlang:get_cookie()
    end,
  true = erlang:set_cookie(LocalNode, Cookie),


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
                els_utils:cmd(
                  "rebar3",
                  [ "shell"
                  , "--sname"
                  , ProjectNode
                  , "--setcookie"
                  , erlang:atom_to_list(Cookie)
                  ]
                )
            end)
  end,

  els_dap_server:send_event(<<"initialized">>, #{}),

  {#{}, State#{project_node => ProjectNode, launch_params => Params}};
handle_request( {<<"configurationDone">>, _Params}
              , #{ project_node := ProjectNode
                 , launch_params := LaunchParams} = State
              ) ->
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
              , #{ project_node := ProjectNode
                 , breakpoints := Breakpoints0} = State
              ) ->
  #{<<"source">> := #{<<"path">> := Path}} = Params,
  SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
  _SourceModified = maps:get(<<"sourceModified">>, Params, false),
  Module = els_uri:module(els_uri:uri(Path)),

  els_distribution_server:wait_connect_and_monitor(ProjectNode),


  {module, Module} = els_dap_rpc:i(ProjectNode, Module),
  Lines = [Line || #{<<"line">> := Line} <- SourceBreakpoints],

  %% purge all breakpoints from the module
  els_dap_rpc:no_break(ProjectNode, Module),
  Breakpoints1 = do_line_breakpoints(ProjectNode, Module, Lines, Breakpoints0),
  BreakpointsRsps = [
      #{<<"verified">> => true, <<"line">> => Line}
      || {{_, Line}, _} <- els_dap_rpc:all_breaks(ProjectNode, Module)
  ],

  FunctionBreaks = get_function_breaks(Module, Breakpoints1),
  Breakpoints2 = do_function_breaks(ProjectNode, Module, FunctionBreaks, Breakpoints1),

  {#{<<"breakpoints">> => BreakpointsRsps}, State#{ breakpoints => Breakpoints2}};
handle_request({<<"setExceptionBreakpoints">>, _Params}, State) ->
  {#{}, State};
handle_request({<<"setFunctionBreakpoints">>, Params}
              , #{ project_node := ProjectNode
                 , breakpoints := Breakpoints0} = State
              ) ->
  FunctionBreakPoints = maps:get(<<"breakpoints">>, Params, []),
  els_distribution_server:wait_connect_and_monitor(ProjectNode),

  MFAs = [
      begin
          Spec = {Mod, _, _} = parse_mfa(MFA),
          els_dap_rpc:i(ProjectNode, Mod),
          Spec
      end
      || #{<<"name">> := MFA, <<"enabled">> := Enabled} <- FunctionBreakPoints,
          Enabled andalso parse_mfa(MFA) =/= error
  ],

  ModFuncBreaks = lists:foldl(
      fun({M, F, A}, Acc) ->
          case Acc of
              #{M := FBreaks} -> Acc#{M => [{F, A} | FBreaks]};
              _ -> Acc#{M => [{F, A}]}
          end
      end,
      #{},
      MFAs
  ),

  els_dap_rpc:no_break(ProjectNode),
  Breakpoints1 = maps:fold(
      fun (Mod, Breaks, Acc) ->
           Acc#{Mod => Breaks#{function => []}}
      end,
      #{},
      Breakpoints0
  ),

  Breakpoints2 = maps:fold(
      fun(Module, FunctionBreaks, Acc) ->
          do_function_breaks(ProjectNode, Module, FunctionBreaks, Acc)
      end,
      Breakpoints1,
      ModFuncBreaks
  ),
  BreakpointsRsps = [
      #{
          <<"verified">> => true,
          <<"line">> => Line,
          <<"source">> => #{<<"path">> => source(Module, ProjectNode)}
      }
      || {{Module, Line}, [Status, _, _, _]} <-
              els_dap_rpc:all_breaks(ProjectNode),
          Status =:= active
  ],

  %% replay line breaks
  Breakpoints3 = maps:fold(
      fun(Module, _, Acc) ->
          Lines = get_line_breaks(Module, Acc),
          do_line_breakpoints(ProjectNode, Module, Lines, Acc)
      end,
      Breakpoints2,
      Breakpoints2
  ),

  {#{<<"breakpoints">> => BreakpointsRsps}, State#{breakpoints => Breakpoints3}};
handle_request({<<"threads">>, _Params}, #{threads := Threads0} = State) ->
  Threads =
    [ #{ <<"id">> => Id
       , <<"name">> => format_term(Pid)
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
                    , scope_bindings := ExistingScopes} = State) ->
  case frame_by_id(FrameId, maps:values(Threads)) of
    undefined -> {#{<<"scopes">> => []}, State};
    Frame ->
      Bindings = maps:get(bindings, Frame),
      Ref = erlang:unique_integer([positive]),
      {#{<<"scopes">> => [
        #{
          <<"name">> => <<"Locals">>,
          <<"presentationHint">> => <<"locals">>,
          <<"variablesReference">> => Ref
        }
      ]}, State#{scope_bindings => ExistingScopes#{Ref => {generic, Bindings}}}}
  end;
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
handle_request({<<"evaluate">>, #{ <<"context">> := Context
                                 , <<"frameId">> := FrameId
                                 , <<"expression">> := Input
                                 } = _Params}
              , #{ threads := Threads } = State
) when Context =:= <<"watch">> orelse Context =:= <<"hover">> ->
  %% hover makes only sense for variables
  %% use the expression as fallback
  case frame_by_id(FrameId, maps:values(Threads)) of
    undefined ->   {#{<<"result">> => <<"not available">>}, State};
    Frame ->
      Bindings = maps:get(bindings, Frame),
      VarName = erlang:list_to_atom(els_utils:to_list(Input)),
      case proplists:lookup(VarName, Bindings) of
        {VarName, VarValue} ->
          build_evaluate_response(VarValue, State);
        none ->
          {#{<<"result">> => <<"not available">>}, State}
      end
  end;
handle_request({<<"evaluate">>, #{ <<"context">> := <<"repl">>
                                 , <<"frameId">> := FrameId
                                 , <<"expression">> := Input
                                 } = _Params}
              , #{ threads := Threads
                 , project_node := ProjectNode
                 } = State
) ->
  %% repl and watch can use whole expressions,
  %% but we still want structured variable scopes
  case pid_by_frame_id(FrameId, maps:values(Threads)) of
    undefined ->
      {#{<<"result">> => <<"not available">>}, State};
    Pid  ->
      {ok, Meta} = els_dap_rpc:get_meta(ProjectNode, Pid),
      Command = els_utils:to_list(Input),
      Return = els_dap_rpc:meta_eval(ProjectNode, Meta, Command),
      build_evaluate_response(Return, State)
  end;
handle_request({<<"variables">>, #{<<"variablesReference">> := Ref
                                  } = _Params}
              , #{ scope_bindings := AllBindings
                 } = State) ->
  #{Ref := {Type, Bindings}} = AllBindings,
  RestBindings = maps:remove(Ref, AllBindings),
  {Variables, MoreBindings} = build_variables(Type, Bindings),
  { #{<<"variables">> => Variables}
  , State#{ scope_bindings => maps:merge(RestBindings, MoreBindings)}};
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
  [{Level, {M, F, A}} | Rest] =
    els_dap_rpc:meta(Node, Meta, backtrace, all),
  Bindings = els_dap_rpc:meta(Node, Meta, bindings, Level),
  StackFrameId = erlang:unique_integer([positive]),
  StackFrame = #{ module    => M
                , function  => F
                , arguments => A
                , source    => source(M, Node)
                , line      => break_line(Pid, Node)
                , bindings  => Bindings},
  collect_frames(Node, Meta, Level, Rest, #{StackFrameId => StackFrame}).

-spec break_line(pid(), atom()) -> integer().
break_line(Pid, Node) ->
  Snapshots = els_dap_rpc:snapshot(Node),
  {Pid, _Function, break, {_Module, Line}} = lists:keyfind(Pid, 1, Snapshots),
  Line.

-spec source(atom(), atom()) -> binary().
source(Module, Node) ->
  CompileOpts = els_dap_rpc:module_info(Node, Module, compile),
  els_dap_rpc:clear(Node),
  Source = proplists:get_value(source, CompileOpts),
  unicode:characters_to_binary(Source).

-spec to_pid(pos_integer(), #{thread_id() => thread()}) -> pid().
to_pid(ThreadId, Threads) ->
  Thread = maps:get(ThreadId, Threads),
  maps:get(pid, Thread).

-spec frame_by_id(frame_id(), [thread()]) -> frame() | undefined.
frame_by_id(FrameId, Threads) ->
  case [  maps:get(FrameId, Frames)
       || #{frames := Frames} <- Threads, maps:is_key(FrameId, Frames)
       ] of
      [Frame] -> Frame;
      _ -> undefined
  end.

-spec pid_by_frame_id(frame_id(), [thread()]) -> pid() | undefined.
pid_by_frame_id(FrameId, Threads) ->
  case [  Pid
       || #{frames := Frames, pid := Pid} <- Threads
       ,  maps:is_key(FrameId, Frames)
       ] of
    [Proc] -> Proc;
    _ ->
      undefined
  end.

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
   , <<"value">> => format_term(Value)
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
            io_lib:format("~s => ~s", [format_term(Key), format_term(Value)])),
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

-spec is_structured(term()) -> boolean().
is_structured(Term) when
  is_list(Term) orelse
  is_map(Term) orelse
  is_tuple(Term) -> true;
is_structured(_) -> false.

-spec build_evaluate_response(term(), state()) -> {any(), state()}.
build_evaluate_response(
  ResultValue,
  State = #{scope_bindings := ExistingScopes}
) ->
  ResultBinary = format_term(ResultValue),
  case is_structured(ResultValue) of
        true ->
          {_, SubScope} = build_variables(generic, [{undefined, ResultValue}]),
          %% there is onlye one sub-scope returned
          [Ref] = maps:keys(SubScope),
          NewScopes = maps:merge(ExistingScopes, SubScope),
          { #{<<"result">> => ResultBinary, <<"variablesReference">> => Ref}
          ,  State#{scope_bindings => NewScopes}
          };
        false ->
          { #{<<"result">> => ResultBinary}
          ,  State
          }
  end.

-spec format_term(term()) -> binary().
format_term(T) ->
  %% print on one line and print strings
  %% as printable characters (if possible)
  els_utils:to_binary(
    [  string:trim(Line)
    || Line <- string:split(io_lib:format("~tp", [T]), "\n", all)]).

-spec collect_frames(node(), pid(), pos_integer(), Backtrace, Acc) -> Acc
  when Acc       :: #{frame_id() => frame()},
       Backtrace :: [{pos_integer(), {module(), atom(), non_neg_integer()}}].
collect_frames(_, _, _, [], Acc) -> Acc;
collect_frames(Node, Meta, Level, [{NextLevel, {M, F, A}} | Rest], Acc) ->
  case els_dap_rpc:meta(Node, Meta, stack_frame, {up, Level}) of
    {NextLevel, {_, Line}, Bindings} ->
      StackFrameId = erlang:unique_integer([positive]),
      StackFrame = #{ module    => M
                    , function  => F
                    , arguments => A
                    , source    => source(M, Node)
                    , line      => Line
                    , bindings  => Bindings},
      collect_frames( Node
                    , Meta
                    , NextLevel
                    , Rest
                    , Acc#{StackFrameId => StackFrame}
                    );
    BadFrame ->
      ?LOG_ERROR( "Received a bad frame: ~p expected level ~p and module ~p"
                , [BadFrame, NextLevel, M]),
      Acc
  end.

%% breakpoint management
-spec get_function_breaks(module(), breakpoints()) -> [function_break()].
get_function_breaks(Module, Breaks) ->
  case Breaks of
    #{Module := #{function := Functions}} -> Functions;
    _ -> []
  end.

-spec get_line_breaks(module(), breakpoints()) -> [line()].
get_line_breaks(Module, Breaks) ->
  case Breaks of
    #{Module := #{line := Lines}} -> Lines;
    _ -> []
  end.

-spec do_line_breakpoints(node(), module(), [line()], breakpoints()) -> breakpoints().
do_line_breakpoints(Node, Module, Lines, Breaks) ->
  [els_dap_rpc:break(Node, Module, Line) || Line <- Lines],
  case Breaks of
    #{Module := ModBreaks} -> Breaks#{ Module => ModBreaks#{line => Lines}};
    _ -> Breaks#{ Module => #{line => Lines, function => []}}
  end.

-spec do_function_breaks(node(), module(), [function_break()], breakpoints()) -> breakpoints().
do_function_breaks(Node, Module, FBreaks, Breaks) ->
  [els_dap_rpc:break_in(Node, Module, Func, Arity) || {Func, Arity} <- FBreaks],
  case Breaks of
    #{Module := ModBreaks} -> Breaks#{ Module => ModBreaks#{function => FBreaks}};
    _ -> Breaks#{ Module => #{line => [], function => FBreaks}}
  end.
