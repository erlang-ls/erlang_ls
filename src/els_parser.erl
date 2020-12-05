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
-spec parse(binary()) -> {ok, [poi()]}.
parse(Text) ->
  IoDevice = els_io_string:new(Text),
  parse_file(IoDevice).

-spec parse_file(file:io_device()) -> {ok, [poi()]}.
parse_file(IoDevice) ->
  {ok, NestedPOIs} = els_dodger:parse(IoDevice, {1, 1}, fun parse_form/3, []),
  ok = file:close(IoDevice),
  {ok, lists:flatten(NestedPOIs)}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% Adapted from els_dodger
-spec parse_form(file:io_device(), any(), [any()]) ->
    {'ok', erl_syntax:forms()
  | none, integer()}
  | {'eof', integer()}
  | {'error', any(), integer()}.
parse_form(IoDevice, Location, Options) ->
  parse_form(IoDevice, Location, fun els_dodger:normal_parser/2, Options).

%% Adapted from els_dodger
-spec parse_form(file:io_device(), any(), function(), [any()]) ->
  {'ok', erl_syntax:forms() | none, integer()}
  | {'eof', integer()}
  | {'error', any(), integer()}.
parse_form(IoDevice, StartLocation, Parser, _Options) ->
  case io:scan_erl_form(IoDevice, "", StartLocation) of
    {ok, Tokens, EndLocation} ->
      try {ok, Parser(Tokens, undefined)} of
        {ok, Tree} ->
          POIs = [ find_attribute_pois(Tree, Tokens)
                 , points_of_interest(Tree, EndLocation)
                 ],
          {ok, POIs, EndLocation}
      catch
        _:_ ->
          {ok, find_attribute_tokens(Tokens), EndLocation}
      end;
    {error, _IoErr, _EndLocation} = Err -> Err;
    {error, _Reason} -> {eof, StartLocation};
    {eof, _EndLocation} = Eof -> Eof
  end.

-spec find_attribute_pois(erl_syntax:syntaxTree(), [erl_scan:token()]) ->
   [poi()].
find_attribute_pois(Tree, Tokens) ->
  case erl_syntax:type(Tree) of
    attribute ->
      try erl_syntax_lib:analyze_attribute(Tree) of
        {export, Exports} ->
          %% The first atom is the attribute name, so we skip it.
          [_|Atoms] = [T || {atom, _, _} = T <- Tokens],
          ExportEntries =
            [ poi(Pos, export_entry, {F, A})
              || {{F, A}, {atom, Pos, _}} <- lists:zip(Exports, Atoms)
            ],
          [find_attribute_tokens(Tokens), ExportEntries];
        {import, {M, Imports}} ->
          %% The first two atoms are the attribute name and the imported
          %% module, so we skip them.
          [_, _|Atoms] = [T || {atom, _, _} = T <- Tokens],
          [ poi(Pos, import_entry, {M, F, A})
            || {{F, A}, {atom, Pos, _}} <- lists:zip(Imports, Atoms)];
        {spec, {spec, {{F, A}, FTs}}} ->
          From = erl_syntax:get_pos(Tree),
          To   = erl_scan:location(lists:last(Tokens)),
          Data = pretty_print_spec(Tree),
          [ poi({From, To}, spec, {F, A}, Data)
          | lists:flatten([find_spec_points_of_interest(FT) || FT <- FTs])
          ];
        {export_type, {export_type, Exports}} ->
          [_ | Atoms] = [T || {atom, _, _} = T <- Tokens],
          ExportTypeEntries =
            [ poi(Pos, export_type_entry, {F, A})
              || {{F, A}, {atom, Pos, _}} <- lists:zip(Exports, Atoms)
            ],
          [find_attribute_tokens(Tokens), ExportTypeEntries];
        _ -> []
      catch
        throw:syntax_error ->
          []
      end;
    _ ->
      []
  end.

%% @doc Resolve POI for specific sections
%%
%% These sections are such things as `export' or `spec' attributes, for which
%% we want to detect their start and end, for example to provide different
%% completion items. Using the tokens provides accurate position for the
%% beginning and end for this sections, and can also handle the situations when
%% the code is not parsable.
-spec find_attribute_tokens([erl_scan:token()]) -> [poi()].
find_attribute_tokens([ {'-', Anno}, {atom, _, Name} | [_|_] = Rest])
  when Name =:= export;
       Name =:= export_type ->
  From = erl_anno:location(Anno),
  To = erl_scan:location(lists:last(Rest)),
  [poi({From, To}, Name, From)];
find_attribute_tokens([ {'-', Anno}, {atom, _, spec} | [_|_] = Rest]) ->
  From = erl_anno:location(Anno),
  To = erl_scan:location(lists:last(Rest)),
  [poi({From, To}, spec, undefined)];
find_attribute_tokens(_) ->
  [].

%% @doc Find points of interest in a spec attribute.
-spec find_spec_points_of_interest(tree()) -> [poi()].
find_spec_points_of_interest(Tree) ->
  fold(fun do_find_spec_points_of_interest/2, [], Tree).

-spec do_find_spec_points_of_interest(tree(), [any()]) -> [poi()].
do_find_spec_points_of_interest(Tree, Acc) ->
  Pos = erl_syntax:get_pos(Tree),
  case {is_type_application(Tree), Tree} of
    {true, {remote_type, _, [Module, Type, Args]}} ->
      Id = { erl_syntax:atom_value(Module)
           , erl_syntax:atom_value(Type)
           , length(Args)
           },
      [poi(Pos, type_application, Id)|Acc];
    {true, {type, _, {type, Type}, Args}} ->
      Id = {Type, length(Args)},
      [poi(Pos, type_application, Id)|Acc];
    {true, {type, _, record, [{atom, _, Name}]}} ->
      [poi(Pos, record_expr, Name)|Acc];
    {true, {user_type, _, Type, Args}} ->
      Id = {Type, length(Args)},
      [poi(Pos, type_application, Id)|Acc];
    _ ->
      Acc
  end.

-spec points_of_interest(tree(), erl_anno:location()) -> [poi()].
points_of_interest(Tree, EndLocation) ->
  FoldFun = fun(T, Acc) -> [do_points_of_interest(T, EndLocation) | Acc] end,
  fold(FoldFun, [], Tree).

%% @doc Return the list of points of interest for a given `Tree'.
-spec do_points_of_interest(tree(), erl_anno:location()) -> [poi()].
do_points_of_interest(Tree, EndLocation) ->
  try
    case erl_syntax:type(Tree) of
      application   -> application(Tree);
      attribute     -> attribute(Tree);
      function      -> function(Tree, EndLocation);
      implicit_fun  -> implicit_fun(Tree);
      macro         -> macro(Tree);
      record_access -> record_access(Tree);
      record_expr   -> record_expr(Tree);
      variable      -> variable(Tree);
      atom          -> atom(Tree);
      _             -> []
    end
  catch throw:syntax_error -> []
  end.

-spec application(tree()) -> [poi()].
application(Tree) ->
  case application_mfa(Tree) of
    undefined -> [];
    {F, A} ->
      Pos = erl_syntax:get_pos(Tree),
      case erl_internal:bif(F, A) of
        %% Call to a function from the `erlang` module
        true -> [poi(Pos, application, {erlang, F, A}, #{imported => true})];
        %% Local call
        false -> [poi(Pos, application, {F, A})]
      end;
    MFA ->
      [poi(erl_syntax:get_pos(Tree), application, MFA)]
  end.

-spec application_mfa(tree()) ->
  {module(), atom(), arity()} | {atom(), arity()} | undefined.
application_mfa(Tree) ->
  case erl_syntax_lib:analyze_application(Tree) of
    %% Remote call
    {M, {F, A}} -> {M, F, A};
    {F, A} -> {F, A};
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
      [poi(Pos, behaviour, Behaviour)];
    {behaviour, {behaviour, Behaviour}} ->
      [poi(Pos, behaviour, Behaviour)];
    {callback, {callback, {{F, A}, _}}} ->
      [poi(Pos, callback, {F, A})];
    {compile, {compile, {parse_transform, ParseTransform}}} ->
      [poi(Pos, parse_transform, ParseTransform)];
    {compile, {compile, CompileOpts}} when is_list(CompileOpts) ->
      [poi(Pos, parse_transform, PT) || {parse_transform, PT} <- CompileOpts];
    {module, {Module, _Args}} ->
      [poi(Pos, module, Module)];
    {module, Module} ->
      [poi(Pos, module, Module)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [poi(Pos, define, define_name(Define))];
        {include, [String]} ->
          [poi(Pos, include, erl_syntax:string_value(String))];
        {include_lib, [String]} ->
          [poi(Pos, include_lib, erl_syntax:string_value(String))];
        _ ->
          []
      end;
    {record, {Record, Fields}} ->
      [poi(Pos, record, Record, Fields)];
    {Name, {Name, {Type, _, Args}}} when Name =:= type;
                                         Name =:= opaque ->
      [poi(Pos, type_definition, {Type, length(Args)}, type_args(Args))];
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec type_args([any()]) -> [{integer(), string()}].
type_args(Args) ->
  [ case erl_syntax:type(T) of
      variable -> {N, erl_syntax:variable_literal(T)};
      _        -> {N, "Type" ++ integer_to_list(N)}
    end
    || {N, T} <- lists:zip(lists:seq(1, length(Args)), Args)
  ].

-spec function(tree(), erl_anno:location()) -> [poi()].
function(Tree, {EndLine, _} = _EndLocation) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  Clauses = erl_syntax:function_clauses(Tree),
  IndexedClauses = lists:zip(lists:seq(1, length(Clauses)), Clauses),
  ClausesPOIs = [ poi(erl_syntax:get_pos(Clause), function_clause, {F, A, I})
                  || {I, Clause} <- IndexedClauses],
  Args = function_args(hd(Clauses), A),
  {StartLine, _} = StartLocation = erl_syntax:get_pos(Tree),
  %% It only makes sense to fold a function if the function contains
  %% at least one line apart from its signature.
  FoldingRanges = case EndLine - StartLine > 1 of
                    true ->
                      Range = #{ from => {StartLine, ?END_OF_LINE}
                               , to   => {EndLine - 1, ?END_OF_LINE}
                               },
                      [ els_poi:new(Range, folding_range, StartLocation) ];
                    false ->
                      []
                  end,
  lists:append([ [ poi(StartLocation, function, {F, A}, Args) ]
               , FoldingRanges
               , ClausesPOIs
               ]).

-spec function_args(tree(), arity()) -> [{integer(), string()}].
function_args(Clause, Arity) ->
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
    _ -> [poi(erl_syntax:get_pos(Tree), implicit_fun, FunSpec)]
  end.

-spec macro(tree()) -> [poi()].
macro(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, macro, node_name(Tree))]
  end.

-spec record_access(tree()) -> [poi()].
record_access(Tree) ->
  RecordNode = erl_syntax:record_access_type(Tree),
  FieldNode = erl_syntax:record_access_field(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_value(RecordNode),
      Field = case erl_syntax:type(FieldNode) of
                atom -> erl_syntax:atom_value(FieldNode);
                _    -> 'UNKNOWN_FIELD'
              end,
      [poi(erl_syntax:get_pos(Tree), record_access, Record, Field)];
    _ ->
      []
  end.

-spec record_expr(tree()) -> [poi()].
record_expr(Tree) ->
  RecordNode = erl_syntax:record_expr_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_value(RecordNode),
      [poi(erl_syntax:get_pos(Tree), record_expr, Record)];
    _ ->
      []
  end.

-spec variable(tree()) -> [poi()].
variable(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, variable, node_name(Tree))]
  end.

-spec atom(tree()) -> [poi()].
atom(Tree) ->
  Pos = erl_syntax:get_pos(Tree),
  case Pos of
    0 -> [];
    _ -> [poi(Pos, atom, node_name(Tree))]
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
      erl_syntax:atom_value(Tree);
    underscore ->
      '_'
  end.

-spec node_name(tree()) -> atom().
node_name(Tree) ->
  case erl_syntax:type(Tree) of
    atom ->
      erl_syntax:atom_value(Tree);
    variable ->
      erl_syntax:variable_name(Tree);
    macro ->
      node_name(erl_syntax:macro_name(Tree));
    underscore ->
      '_'
  end.

-spec is_type_application(tree()) -> boolean().
is_type_application(Tree) ->
  Type  = erl_syntax:type(Tree),
  Types = [type_application, user_type_application, record_type],
  lists:member(Type, Types).

-spec poi(pos() | {pos(), pos()}, poi_kind(), any()) -> poi().
poi(Pos, Kind, Id) ->
  poi(Pos, Kind, Id, undefined).

-spec poi(pos() | {pos(), pos()}, poi_kind(), any(), any()) ->
  poi().
poi(Pos, Kind, Id, Data) ->
  Range = els_range:range(Pos, Kind, Id, Data),
  els_poi:new(Range, Kind, Id, Data).

%% @doc Fold over nodes in the tree
%%
%% Modified version of `erl_syntax_lib:fold/3', to get control over
%% what subtrees should be folded over for certain types of nodes.
-spec fold(fun((tree(), term()) -> term()), term(), tree()) -> term().
fold(F, S, Tree) ->
  case subtrees(Tree, erl_syntax:type(Tree)) of
    [] -> F(Tree, S);
    Gs -> F(Tree, fold_1(F, S, Gs))
  end.

-spec fold_1(fun((tree(), term()) -> term()), term(), [[tree()]]) ->
  term().
fold_1(F, S, [L | Ls]) ->
  fold_1(F, fold_2(F, S, L), Ls);
fold_1(_, S, []) ->
  S.

-spec fold_2(fun((tree(), term()) -> term()), term(), [tree()]) ->
  term().
fold_2(F, S, [T | Ts]) ->
  fold_2(F, fold(F, S, T), Ts);
fold_2(_, S, []) ->
  S.

-spec subtrees(tree(), atom()) -> [[tree()]].
subtrees(Tree, application) ->
  [erl_syntax:application_arguments(Tree)];
subtrees(Tree, function) ->
  [erl_syntax:function_clauses(Tree)];
subtrees(_Tree, implicit_fun) ->
  [];
subtrees(Tree, macro) ->
  case erl_syntax:macro_arguments(Tree) of
    none -> [];
    Args -> [Args]
  end;
subtrees(Tree, record_access) ->
  [ [ erl_syntax:record_access_argument(Tree)
    , erl_syntax:record_access_field(Tree)
    ]
  ];
subtrees(Tree, record_expr) ->
  Fields = erl_syntax:record_expr_fields(Tree),
  case erl_syntax:record_expr_argument(Tree) of
    none -> [Fields];
    Arg  -> [[Arg], Fields]
  end;
subtrees(Tree, attribute) ->
  case erl_syntax:attribute_arguments(Tree) of
    none -> [];
    [_ | RestArgs] -> [RestArgs]
  end;
subtrees(Tree, _) ->
  erl_syntax:subtrees(Tree).

-spec pretty_print_spec(tree()) -> binary().
pretty_print_spec(Tree) ->
  try
    els_utils:to_binary(erl_prettypr:format(Tree))
 catch
   _:_:_ ->
     <<>>
 end.
