-module(els_dap_breakpoints).
-export([ build_source_breakpoints/1
        , get_function_breaks/2
        , get_line_breaks/2
        , do_line_breakpoints/4
        , do_function_breaks/4
        , type/3]).

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% Types
%%==============================================================================

-type breakpoints() :: #{
  module() => #{
      line => #{
        line() => line_breaks()
      },
      function => [function_break()]
  }
}.
-type line() :: non_neg_integer().
-type line_breaks() ::
    regular
  | {log, expression()}.
-type expression() :: string().
-type function_break() :: {atom(), non_neg_integer()}.

-export_type([breakpoints/0]).

-spec type(breakpoints(), module(), line()) -> line_breaks().
type(Breakpoints, Module, Line) ->
  ?LOG_DEBUG("checking breakpoint type for ~s:~b", [Module, Line]),
  case Breakpoints of
    #{Module := #{line := #{Line := Break}}} ->
      Break;
    _ ->
      %% function breaks get handled like regular ones
      regular
  end.

%% @doc build regular and log breakpoints from setBreakpoint request
-spec build_source_breakpoints(Params :: map()) ->
        {module(), #{line() => line_breaks()}}.
build_source_breakpoints(Params) ->
  #{<<"source">> := #{<<"path">> := Path}} = Params,
  Module = els_uri:module(els_uri:uri(Path)),
  SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
  _SourceModified = maps:get(<<"sourceModified">>, Params, false),
  {Module, maps:from_list(lists:map(fun build_source_breakpoint/1,
                                    SourceBreakpoints))}.

-spec build_source_breakpoint(map()) ->
        {line(), 'regular' | {'log', expression()}}.
build_source_breakpoint(#{<<"line">> := Line, <<"logMessage">> := LogExpr}) ->
  {Line, {log, LogExpr}};
build_source_breakpoint(#{<<"line">> := Line}) ->
  {Line, regular}.

-spec get_function_breaks(module(), breakpoints()) -> [function_break()].
get_function_breaks(Module, Breaks) ->
  case Breaks of
    #{Module := #{function := Functions}} -> Functions;
    _ -> []
  end.

-spec get_line_breaks(module(), breakpoints()) -> #{line() => line_breaks()}.
get_line_breaks(Module, Breaks) ->
  case Breaks of
    #{Module := #{line := Lines}} -> Lines;
    _ -> []
  end.

-spec do_line_breakpoints(node(), module(),
                          #{line() => line_breaks()}, breakpoints()) ->
  breakpoints().
do_line_breakpoints(Node, Module, LineBreakPoints, Breaks) ->
  maps:map(
    fun
      (Line, regular) -> els_dap_rpc:break(Node, Module, Line);
      (Line, {log, _}) -> els_dap_rpc:break(Node, Module, Line)
    end,
    LineBreakPoints
  ),
  case Breaks of
    #{Module := ModBreaks} ->
      Breaks#{Module => ModBreaks#{line => LineBreakPoints}};
    _ ->
      Breaks#{Module => #{line => LineBreakPoints, function => []}}
  end.

-spec do_function_breaks(node(), module(), [function_break()], breakpoints()) ->
        breakpoints().
do_function_breaks(Node, Module, FBreaks, Breaks) ->
  [els_dap_rpc:break_in(Node, Module, Func, Arity) || {Func, Arity} <- FBreaks],
  case Breaks of
    #{Module := ModBreaks} ->
      Breaks#{Module => ModBreaks#{function => FBreaks}};
    _ ->
      Breaks#{Module => #{line => #{}, function => FBreaks}}
  end.
