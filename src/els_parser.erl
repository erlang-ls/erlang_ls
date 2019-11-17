%%==============================================================================
%% The erlang_ls parser. It uses the epp_dodger OTP library.
%%==============================================================================
-module(els_parser).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ parse/1
        , parse_file/1
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================
-spec parse(binary()) -> {ok, [poi()]} | {error, any()}.
parse(Text) ->
  %% epp_dodger only works with source files,
  %% so let's use a temporary file.
  %% TODO: Ideally avoid writing to file at all (require epp changes)
  TmpPath = tmp_path(),
  try
    ok = file:write_file(TmpPath, Text),
    parse_file(TmpPath)
  after
    file:delete(TmpPath)
  end.

-spec parse_file(binary()) -> {ok, [poi()]} | {error, any()}.
parse_file(Path) ->
  case file:open(Path, [read]) of
    {ok, IoDevice} ->
      %% Providing `{1, 1}` as the initial location ensures
      %% that the returned forms include column numbers, as well.
      %% The specs for the epp_dodger API are slightly incorrect.
      %% A bug has been reported (see https://bugs.erlang.org/browse/ERL-1005)
      %% Meanwhile, let's trick Dialyzer with an apply.
      {ok, Forms} = erlang:apply(epp_dodger, parse, [IoDevice, {1, 1}]),
      Tree = erl_syntax:form_list(Forms),
      %% Reset file pointer position.
      {ok, 0} = file:position(IoDevice, 0),
      {ok, Extra} = parse_attribute_pois(IoDevice, [], {1, 1}),
      ok = file:close(IoDevice),
      POIs = points_of_interest(Tree),
      {ok, Extra ++ POIs};
    {error, Error} ->
      {error, Error}
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec parse_attribute_pois(io:device(), [poi()], erl_anno:location()) ->
   {ok, [poi()]} | {error, any()}.
parse_attribute_pois(IoDevice, Acc, StartLoc) ->
  case io:scan_erl_form(IoDevice, "", StartLoc) of
    {ok, Tokens, EndLoc} ->
      case erl_parse:parse_form(Tokens) of
        {ok, Form} ->
          POIs = find_attribute_pois(Form, Tokens),
          parse_attribute_pois(IoDevice, POIs ++ Acc, EndLoc);
        {error, _Error} ->
          parse_attribute_pois(IoDevice, Acc, EndLoc)
      end;
    {eof, _} ->
      {ok, Acc};
    {error, ErrorInfo, EndLoc} ->
      lager:warning( "Could not parse extra information [end_loc=p] ~p"
                   , [EndLoc, ErrorInfo]
                   ),
      {ok, Acc}
  end.

-spec find_attribute_pois(erl_parse:abstract_form(), [erl_scan:token()]) ->
   [poi()].
find_attribute_pois(Form, Tokens) ->
  case erl_syntax:type(Form) of
    attribute ->
      case erl_syntax_lib:analyze_attribute(Form) of
        {export, Exports} ->
          %% The first atom is the attribute name, so we skip it.
          [_|Atoms] = [T|| {atom, _, _} = T <- Tokens],
          [ els_poi:new(Pos, export_entry, {F, A})
            || {{F, A}, {atom, Pos, _}} <- lists:zip(Exports, Atoms)];
        {import, {M, Imports}} ->
          %% The first two atoms are the attribute name and the imported
          %% module, so we skip them.
          [_, _|Atoms] = [T|| {atom, _, _} = T <- Tokens],
          [ els_poi:new(Pos, import_entry, {M, F, A})
            || {{F, A}, {atom, Pos, _}} <- lists:zip(Imports, Atoms)];
        {spec, {spec, {_, FTs}}} ->
          lists:flatten([find_spec_points_of_interest(FT) || FT <- FTs]);
        _ ->
          []
      end;
    _ ->
      []
  end.

%% @edoc Find points of interest in a spec attribute.
-spec find_spec_points_of_interest(tree()) -> [poi()].
find_spec_points_of_interest(Tree) ->
  Fun = fun do_find_spec_points_of_interest/2,
  Res = erl_syntax_lib:fold(Fun, [], Tree),
  Res.

-spec do_find_spec_points_of_interest(tree(), [any()]) -> [poi()].
do_find_spec_points_of_interest(Tree, Acc) ->
  Pos = erl_syntax:get_pos(Tree),
  case {is_type_application(Tree), Tree} of
    {true, {remote_type, _, [Module, Type, Args]}} ->
      Id = { erl_syntax:atom_value(Module)
           , erl_syntax:atom_value(Type)
           , length(Args)
           },
      [els_poi:new(Pos, type_application, Id)|Acc];
    {true, {type, _, {type, Type}, Args}} ->
      Id = {Type, length(Args)},
      [els_poi:new(Pos, type_application, Id)|Acc];
    {true, {user_type, _, Type, Args}} ->
      Id = {Type, length(Args)},
      [els_poi:new(Pos, type_application, Id)|Acc];
    _ ->
      Acc
  end.

-spec tmp_path() -> binary().
tmp_path() ->
  RandBin = integer_to_binary(erlang:unique_integer([positive, monotonic])),
  <<"/tmp/els_tmp_", RandBin/binary>>.

-spec points_of_interest(tree()) -> [poi()].
points_of_interest(Tree) ->
  lists:flatten(
    erl_syntax_lib:fold(
      fun(T, Acc) ->
          [do_points_of_interest(T)|Acc]
      end, [], Tree)).

%% @edoc Return the list of points of interest for a given `Tree`.
-spec do_points_of_interest(tree()) -> [poi()].
do_points_of_interest(Tree) ->
  case erl_syntax:type(Tree) of
    application   -> application(Tree);
    attribute     -> attribute(Tree);
    function      -> function(Tree);
    implicit_fun  -> implicit_fun(Tree);
    macro         -> macro(Tree);
    record_access -> record_access(Tree);
    record_expr   -> record_expr(Tree);
    variable      -> variable(Tree);
    _             -> []
  end.

-spec application(tree()) -> [poi()].
application(Tree) ->
  case application_mfa(Tree) of
    undefined -> [];
    MFA -> [els_poi:new(erl_syntax:get_pos(Tree), application, MFA)]
  end.

-spec application_mfa(tree()) ->
  {module(), atom(), arity()} | {atom(), arity()} | undefined.
application_mfa(Tree) ->
  case erl_syntax_lib:analyze_application(Tree) of
    %% Remote call
    {M, {F, A}} -> {M, F, A};
    {F, A} ->
      case lists:member({F, A}, erlang:module_info(exports)) of
        %% Call to a function from the `erlang` module
              true -> {erlang, F, A};
        %% Local call
        false -> {F, A}
      end;
    A when is_integer(A) ->
      %% If the function is not explicitly named (e.g. a variable is
      %% used as the module qualifier or the function name), only the
      %% arity A is returned.
      %% In the special case where the macro `?MODULE` is used as the
      %% module qualifier, we can consider it as a local call.
      Operator = erl_syntax:application_operator(Tree),
      case erl_syntax:type(Operator) of
        module_qualifier -> application_with_variable(Operator, A);
        _                -> undefined
      end
  end.

-spec application_with_variable(tree(), arity()) ->
  {atom(), arity()} | undefined.
application_with_variable(Operator, A) ->
  Module   = erl_syntax:module_qualifier_argument(Operator),
  Function = erl_syntax:module_qualifier_body(Operator),
  case {erl_syntax:type(Module), erl_syntax:type(Function)} of
    %% The usage of the ?MODULE macro as the module name for
    %% fully qualified calls is so common that it is worth a
    %% specific clause.
    {macro, atom} ->
      ModuleName   = node_name(Module),
      FunctionName = node_name(Function),
      case {ModuleName, FunctionName} of
        {'MODULE', F} -> {F, A};
        _ -> undefined
      end;
    _ -> undefined
  end.

-spec attribute(tree()) -> [poi()].
attribute(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  try erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [els_poi:new(Pos, behaviour, Behaviour)];
    {behaviour, {behaviour, Behaviour}} ->
      [els_poi:new(Pos, behaviour, Behaviour)];
    {module, {Module, _Args}} ->
      [els_poi:new(Pos, module, Module)];
    {module, Module} ->
      [els_poi:new(Pos, module, Module)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [els_poi:new(Pos, define, define_name(Define))];
        {include, [String]} ->
          [els_poi:new(Pos, include, erl_syntax:string_value(String))];
        {include_lib, [String]} ->
          [els_poi:new(Pos, include_lib, erl_syntax:string_value(String))];
        _ ->
          []
      end;
    {record, {Record, _Fields}} ->
      [els_poi:new(Pos, record, atom_to_list(Record))];
    {spec, {spec, {{F, A}, _}}} ->
      [els_poi:new(Pos, spec, {F, A}, Tree)];
    {type, {type, {Type, _, Args}}} ->
      [els_poi:new(Pos, type_definition, {Type, length(Args)})];
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec function(tree()) -> [poi()].
function(Tree) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  Args   = function_args(Tree, A),
  [els_poi:new(erl_syntax:get_pos(Tree), function, {F, A}, Args)].

-spec function_args(tree(), arity()) -> [{integer(), string()}].
function_args(Tree, Arity) ->
  Clause   = hd(erl_syntax:function_clauses(Tree)),
  Patterns = erl_syntax:clause_patterns(Clause),
  [ case erl_syntax:type(P) of
      variable -> {N, erl_syntax:variable_literal(P)};
      _        -> {N, "Arg" ++ integer_to_list(N)}
    end
    || {N, P} <- lists:zip(lists:seq(1, Arity), Patterns)
  ].

-spec implicit_fun(tree()) -> [poi()].
implicit_fun(Tree) ->
  FunSpec = try erl_syntax_lib:analyze_implicit_fun(Tree) of
              {M, {F, A}} -> {M, F, A};
              {F, A} -> {F, A}
            catch throw:syntax_error ->
                undefined
            end,
  case FunSpec of
    undefined -> [];
    _ -> [els_poi:new(erl_syntax:get_pos(Tree), implicit_fun, FunSpec)]
  end.

-spec macro(tree()) -> [poi()].
macro(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [els_poi:new(Pos, macro, node_name(Tree))]
  end.

-spec record_access(tree()) -> [poi()].
record_access(Tree) ->
  RecordNode = erl_syntax:record_access_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      Field = erl_syntax:atom_name(erl_syntax:record_access_field(Tree)),
      [els_poi:new(erl_syntax:get_pos(Tree), record_access, {Record, Field})];
    _ ->
      []
  end.

-spec record_expr(tree()) -> [poi()].
record_expr(Tree) ->
  RecordNode = erl_syntax:record_expr_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      [els_poi:new(erl_syntax:get_pos(Tree), record_expr, Record)];
    _ ->
      []
  end.

-spec variable(tree()) -> [poi()].
variable(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [els_poi:new(Pos, variable, node_name(Tree))]
  end.

-spec define_name(tree()) -> atom().
define_name(Tree) ->
  case erl_syntax:type(Tree) of
    application ->
      Operator = erl_syntax:application_operator(Tree),
      node_name(Operator);
    variable ->
      erl_syntax:variable_name(Tree);
    atom ->
      erl_syntax:atom_value(Tree)
  end.

-spec node_name(tree()) -> atom().
node_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_value(Tree);
    variable ->
      erl_syntax:variable_name(Tree);
    macro ->
      node_name(erl_syntax:macro_name(Tree))
  end.

-spec is_type_application(tree()) -> boolean().
is_type_application(Tree) ->
  Type  = erl_syntax:type(Tree),
  Types = [type_application, user_type_application],
  lists:member(Type, Types).
