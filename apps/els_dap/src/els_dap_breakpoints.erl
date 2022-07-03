-module(els_dap_breakpoints).
-export([
    build_source_breakpoints/1,
    get_function_breaks/2,
    get_line_breaks/2,
    do_line_breakpoints/4,
    do_function_breaks/4,
    type/3
]).

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
-type line_breaks() :: #{
    condition => expression(),
    hitcond => expression(),
    logexpr => expression()
}.
-type expression() :: string().
-type function_break() :: {atom(), non_neg_integer()}.

-export_type([
    breakpoints/0,
    line_breaks/0
]).

-spec type(breakpoints(), module(), line()) -> line_breaks().
type(Breakpoints, Module, Line) ->
    ?LOG_DEBUG("checking breakpoint type for ~s:~b", [Module, Line]),
    case Breakpoints of
        #{Module := #{line := #{Line := Break}}} ->
            Break;
        _ ->
            %% function breaks get handled like regular ones
            #{}
    end.

%% @doc build regular, conditional, hit and log breakpoints from setBreakpoint
%% request
-spec build_source_breakpoints(Params :: map()) ->
    {module(), #{line() => line_breaks()}}.
build_source_breakpoints(Params) ->
    #{<<"source">> := #{<<"path">> := Path}} = Params,
    Module = els_uri:module(els_uri:uri(Path)),
    SourceBreakpoints = maps:get(<<"breakpoints">>, Params, []),
    _SourceModified = maps:get(<<"sourceModified">>, Params, false),
    {Module,
        maps:from_list(
            lists:map(
                fun build_source_breakpoint/1,
                SourceBreakpoints
            )
        )}.

-spec build_source_breakpoint(map()) ->
    {line(), #{
        condition => expression(),
        hitcond => expression(),
        logexpr => expression()
    }}.
build_source_breakpoint(#{<<"line">> := Line} = Breakpoint) ->
    Cond =
        case find_non_null_key(<<"condition">>, Breakpoint) of
            {ok, CondExpr} ->
                #{condition => CondExpr};
            error ->
                #{}
        end,
    Hit =
        case find_non_null_key(<<"hitCondition">>, Breakpoint) of
            {ok, HitExpr} ->
                #{hitcond => HitExpr};
            error ->
                #{}
        end,
    Log =
        case find_non_null_key(<<"logMessage">>, Breakpoint) of
            {ok, LogExpr} ->
                #{logexpr => LogExpr};
            error ->
                #{}
        end,
    {Line, lists:foldl(fun maps:merge/2, #{}, [Cond, Hit, Log])}.

%% @doc Finds the `Value' of `Key' in `Map' if it exists and is non-null and
%% non-empty.
%%
%% @returns `{ok, Value}' when `Key' is present in `Map' and the corresponding
%% `Value' is not `null' or the empty binary, `error' otherwise.
-spec find_non_null_key(Key, Map) -> {ok, Value} | error when
    Map :: map(),
    Key :: term(),
    Value :: term().
find_non_null_key(Key, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} when Value =/= <<>> andalso Value =/= null ->
            {ok, Value};
        _ ->
            error
    end.

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

-spec do_line_breakpoints(
    node(),
    module(),
    #{line() => line_breaks()},
    breakpoints()
) ->
    breakpoints().
do_line_breakpoints(Node, Module, LineBreakPoints, Breaks) ->
    maps:map(
        fun(Line, _) -> els_dap_rpc:break(Node, Module, Line) end,
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
