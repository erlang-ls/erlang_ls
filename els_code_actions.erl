-module(els_code_actions).

-export([ export_function/3
        , fix_module_name/3
        , ignore_variable/3
        , suggest_variable/3
        ]).

-include("els_lsp.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec export_function(uri(), range(), [binary()]) -> [map()].
export_function(Uri, _Range, [UnusedFun]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  case els_poi:sort(els_dt_document:pois(Document, [module, export])) of
    [] ->
      [];
    POIs ->
      #{range := #{to := {Line, _Col}}} = lists:last(POIs),
      Pos = {Line + 1, 1},
      [ make_edit_action( Uri
                        , <<"Export ", UnusedFun/binary>>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , <<"-export([", UnusedFun/binary, "]).\n">>
                        , els_protocol:range(#{from => Pos, to => Pos})) ]
  end.

-spec ignore_variable(uri(), range(), [binary()]) -> [map()].
ignore_variable(Uri, Range, [UnusedVariable]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
  case ensure_range(els_range:to_poi_range(Range), UnusedVariable, POIs) of
    {ok, VarRange} ->
      [ make_edit_action( Uri
                        , <<"Add '_' to '", UnusedVariable/binary, "'">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , <<"_", UnusedVariable/binary>>
                        , els_protocol:range(VarRange)) ];
    error ->
      []
  end.

-spec suggest_variable(uri(), range(), [binary()]) -> [map()].
suggest_variable(Uri, Range, [Var]) ->
  %% Supply a quickfix to replace an unbound variable with the most similar
  %% variable name in scope.
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
  case ensure_range(els_range:to_poi_range(Range), Var, POIs) of
    {ok, VarRange} ->
      ScopeRange = els_scope:variable_scope_range(VarRange, Document),
      VarsInScope = [atom_to_binary(Id, utf8) ||
                      #{range := R, id := Id} <- POIs,
                      els_range:in(R, ScopeRange),
                      els_range:compare(R, VarRange)],
      case [{els_utils:levenshtein_distance(V, Var), V} ||
             V <- VarsInScope,
             V =/= Var,
             binary:at(Var, 0) =:= binary:at(V, 0)]
      of
        [] ->
          [];
        VariableDistances ->
          {_, SimilarVariable} = lists:min(VariableDistances),
          [ make_edit_action( Uri
                            , <<"Did you mean '", SimilarVariable/binary, "'?">>
                            , ?CODE_ACTION_KIND_QUICKFIX
                            , SimilarVariable
                            , els_protocol:range(VarRange)) ]
      end;
    error ->
      []
  end.

-spec fix_module_name(uri(), range(), [binary()]) -> [map()].
fix_module_name(Uri, Range0, [ModName, FileName]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [module])),
  case ensure_range(els_range:to_poi_range(Range0), ModName, POIs) of
    {ok, Range} ->
      [ make_edit_action( Uri
                        , <<"Change to -module(", FileName/binary, ").">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , FileName
                        , els_protocol:range(Range)) ];
    error ->
      []
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec ensure_range(poi_range(), binary(), [poi()]) -> {ok, poi_range()} | error.
ensure_range(#{from := {Line, _}}, SubjectId, POIs) ->
  SubjectAtom = binary_to_atom(SubjectId, utf8),
  Ranges = [R || #{range := R, id := Id} <- POIs,
                 els_range:in(R, #{from => {Line, 1}, to => {Line + 1, 1}}),
                 Id =:= SubjectAtom],
  case Ranges of
    [] ->
      error;
    [Range|_] ->
      {ok, Range}
  end.

-spec make_edit_action(uri(), binary(), binary(), binary(), range())
                      -> map().
make_edit_action(Uri, Title, Kind, Text, Range) ->
  #{ title => Title
   , kind => Kind
   , edit => edit(Uri, Text, Range)
   }.

-spec edit(uri(), binary(), range()) -> workspace_edit().
edit(Uri, Text, Range) ->
  #{changes => #{Uri => [#{newText => Text, range => Range}]}}.
