%%==============================================================================
%% The erlang_ls parser. It uses the epp_dodger OTP library.
%%==============================================================================
-module(erlang_ls_parser).

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
      {ok, Extra} = parse_extra(IoDevice, #{}, {1, 1}),
      ok = file:close(IoDevice),
      {ok, points_of_interest(Tree, Extra)};
    {error, Error} ->
      {error, Error}
  end.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec parse_extra(io:device(), extra(), erl_anno:location()) ->
   {ok, extra()} | {error, any()}.
parse_extra(IoDevice, Extra, StartLocation) ->
  case io:scan_erl_form(IoDevice, "", StartLocation) of
    {ok, Tokens, EndLocation} ->
      case erl_parse:parse_form(Tokens) of
        {ok, Form} ->
          parse_extra(IoDevice, extra(Form, Tokens, Extra), EndLocation);
        {error, _Error} ->
          parse_extra(IoDevice, Extra, EndLocation)
      end;
    {eof, _} ->
      {ok, Extra};
    {error, ErrorInfo, EndLocation} ->
      {error, {ErrorInfo, EndLocation}}
  end.

-spec extra( erl_parse:abstract_form()
           , [erl_scan:token()]
           , extra()) -> extra().
extra(Form, Tokens, Extra) ->
  Type = erl_syntax:type(Form),
  extra(Form, Tokens, Extra, Type).

-spec extra(erl_parse:abstract_form(), [erl_scan:token()], extra(), atom()) ->
   extra().
extra(Form, Tokens, Extra, attribute) ->
  case erl_syntax_lib:analyze_attribute(Form) of
    {export, Exports} ->
      %% TODO: Use maps:update_with
      OldLocations = maps:get(exports_locations, Extra, []),
      %% Hackity-hack. The first atom is the attribute name.
      %% We should find a nicer way to parse the export list.
      [_|Locations] = [L || {atom, L, _F} <- Tokens],
      NewLocations = lists:append( OldLocations
                                 , lists:zip(Exports, Locations)
                                 ),
      maps:put(exports_locations, NewLocations, Extra);
    {import, {_M, Imports}} ->
      %% Hackity-hack. The first two atoms are the attribute name and
      %% the import module. We should find a nicer way to parse the
      %% import list.
      OldLocations = maps:get(import_locations, Extra, []),
      [_, _|Locations] = [L || {atom, L, _F} <- Tokens],
      NewLocations = lists:append( OldLocations
                                 , lists:zip(Imports, Locations)
                                 ),
      maps:put(import_locations, NewLocations, Extra);
    {spec, {spec, {{F, A}, FTs}}} ->
      SpecLocations = [spec_locations(FT) || FT <- FTs],
      OldLocations  = maps:get(spec_locations, Extra, []),
      NewLocations  = [{{F, A}, lists:append(SpecLocations)} | OldLocations],
      maps:put(spec_locations, NewLocations, Extra);
    _ ->
      Extra
  end;
extra(_Form, _Tokens, Extra, _Type) ->
  Extra.

-spec spec_locations(tree()) -> [{atom(), erl_anno:location()}].
spec_locations(FT) ->
  case erl_syntax:type(FT) of
    function_type ->
      FTR = erl_syntax:function_type_return(FT),
      FTA = erl_syntax:function_type_arguments(FT),
      do_spec_locations([FTR | FTA], []);
    constrained_function_type ->
      %% TODO
      []
  end.

-spec do_spec_locations([any()], [{atom(), erl_anno:location()}]) ->
   [{atom(), erl_anno:location()}].
do_spec_locations(any, Acc) ->
  Acc;
do_spec_locations([], Acc) ->
  Acc;
do_spec_locations([{type, StartLocation, Type, any}|T], Acc) ->
  do_spec_locations(T, [{Type, StartLocation}|Acc]);
do_spec_locations([{type, StartLocation, Type, Args}|T], Acc) ->
  do_spec_locations(T ++ Args, [{Type, StartLocation}|Acc]);
do_spec_locations([{user_type, StartLocation, Type, any}|T], Acc) ->
  do_spec_locations(T, [{Type, StartLocation}|Acc]);
do_spec_locations([{user_type, StartLocation, Type, Args}|T], Acc) ->
  do_spec_locations(T ++ Args, [{Type, StartLocation}|Acc]);
do_spec_locations([_Else|T], Acc) ->
  do_spec_locations(T, Acc).

-spec tmp_path() -> binary().
tmp_path() ->
  RandBin = integer_to_binary(erlang:unique_integer([positive, monotonic])),
  <<"/tmp/erlang_ls_tmp_", RandBin/binary>>.

-spec points_of_interest(tree(), extra()) -> [poi()].
points_of_interest(Tree, Extra) ->
  lists:flatten(
    erl_syntax_lib:fold(
      fun(T, Acc) ->
          [do_points_of_interest(T, Extra)|Acc]
      end, [], Tree)).


%% @edoc Return the list of points of interest for a given `Tree`.
-spec do_points_of_interest(tree(), extra()) -> [poi()].
do_points_of_interest(Tree, Extra) ->
  try
    case erl_syntax:type(Tree) of
      application   -> application(Tree, Extra);
      attribute     -> attribute(Tree, Extra);
      function      -> function(Tree, Extra);
      implicit_fun  -> implicit_fun(Tree, Extra);
      macro         -> macro(Tree, Extra);
      record_access -> record_access(Tree, Extra);
      record_expr   -> record_expr(Tree, Extra);
      variable      -> variable(Tree, Extra);
      _             -> []
    end
  catch
    Class:Reason:Stacktrace ->
      Position = erl_syntax:get_pos(Tree),
      lager:warning( "Could not analyze tree - error=~p:~p pos=~p "
                     "stacktrace=~p"
                   , [Class, Reason, Position, Stacktrace]),
      []
  end.

-spec application(tree(), extra()) -> [poi()].
application(Tree, Extra) ->
  case application_mfa(Tree) of
    undefined -> [];
    MFA -> [erlang_ls_poi:new(Tree, application, MFA, Extra)]
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

-spec attribute(tree(), extra()) -> [poi()].
attribute(Tree, Extra) ->
  try erl_syntax_lib:analyze_attribute(Tree) of
    %% Yes, Erlang allows both British and American spellings for
    %% keywords.
    {behavior, {behavior, Behaviour}} ->
      [erlang_ls_poi:new(Tree, behaviour, Behaviour, Extra)];
    {behaviour, {behaviour, Behaviour}} ->
      [erlang_ls_poi:new(Tree, behaviour, Behaviour, Extra)];
    {export, Exports} ->
      [erlang_ls_poi:new(Tree, exports_entry, {F, A}, Extra)
       || {F, A} <- Exports];
    {import, {M, Imports}} ->
      [erlang_ls_poi:new(Tree, import_entry, {M, F, A}, Extra)
       || {F, A} <- Imports];
    {module, {Module, _Args}} ->
      [erlang_ls_poi:new(Tree, module, Module, Extra)];
    {module, Module} ->
      [erlang_ls_poi:new(Tree, module, Module, Extra)];
    preprocessor ->
      Name = erl_syntax:atom_value(erl_syntax:attribute_name(Tree)),
      case {Name, erl_syntax:attribute_arguments(Tree)} of
        {define, [Define|_]} ->
          [erlang_ls_poi:new( Tree
                            , define
                            , define_name(Define)
                            , Extra )];
        {include, [String]} ->
          [erlang_ls_poi:new( Tree
                            , include
                            , erl_syntax:string_literal(String)
                            , Extra )];
        {include_lib, [String]} ->
          [erlang_ls_poi:new( Tree
                            , include_lib
                            , erl_syntax:string_literal(String)
                            , Extra )];
        _ ->
          []
      end;
    {record, {Record, _Fields}} ->
      [erlang_ls_poi:new(Tree, record, atom_to_list(Record), Extra)];
    {spec, {spec, {{F, A}, _}}} ->
      SpecLocations = maps:get(spec_locations, Extra, []),
      Locations     = proplists:get_value({F, A}, SpecLocations),
      [ erlang_ls_poi:new(Tree, spec, {{F, A}, Tree}, Extra)
      | [erlang_ls_poi:new(Tree, type_application, {T, L}, Extra)
         || {T, L} <- Locations]
      ];
    {type, {type, Type}} ->
      %% TODO: Support type usages in type definitions
      TypeName = element(1, Type),
      [erlang_ls_poi:new(Tree, type_definition, TypeName, Extra)];
    _ ->
      []
  catch throw:syntax_error ->
      []
  end.

-spec function(tree(), extra()) -> [poi()].
function(Tree, Extra) ->
  {F, A} = erl_syntax_lib:analyze_function(Tree),
  [erlang_ls_poi:new(Tree, function, {F, A}, Extra)].

-spec implicit_fun(tree(), extra()) -> [poi()].
implicit_fun(Tree, Extra) ->
  FunSpec = try erl_syntax_lib:analyze_implicit_fun(Tree) of
              {M, {F, A}} -> {M, F, A};
              {F, A} -> {F, A}
            catch throw:syntax_error ->
                undefined
            end,
  case FunSpec of
    undefined -> [];
    _ -> [erlang_ls_poi:new(Tree, implicit_fun, FunSpec, Extra)]
  end.

-spec macro(tree(), extra()) -> [poi()].
macro(Tree, Extra) ->
  case erl_syntax:get_pos(Tree) of
    0 -> [];
    _ -> [erlang_ls_poi:new(Tree, macro, node_name(Tree), Extra)]
  end.

-spec record_access(tree(), extra()) -> [poi()].
record_access(Tree, Extra) ->
  RecordNode = erl_syntax:record_access_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      Field = erl_syntax:atom_name(erl_syntax:record_access_field(Tree)),
      [erlang_ls_poi:new(Tree, record_access, {Record, Field}, Extra)];
    _ ->
      []
  end.

-spec record_expr(tree(), extra()) -> [poi()].
record_expr(Tree, Extra) ->
  RecordNode = erl_syntax:record_expr_type(Tree),
  case erl_syntax:type(RecordNode) of
    atom ->
      Record = erl_syntax:atom_name(RecordNode),
      [erlang_ls_poi:new(Tree, record_expr, Record, Extra)];
    _ ->
      []
  end.

-spec variable(tree(), extra()) -> [poi()].
variable(Tree, Extra) ->
  case erl_syntax:get_pos(Tree) of
    0 -> [];
    _ -> [erlang_ls_poi:new(Tree, variable, node_name(Tree), Extra)]
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

%% TODO: Support type
%% TODO: Support remote_type
%% TODO: Support tuple
%% TODO: Support union
%% TODO: Add arity to type_definition
%% TODO: Check why proc_lib fails to parse
