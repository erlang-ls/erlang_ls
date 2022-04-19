-module(els_code_action_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

-include("els_lsp.hrl").

-type state() :: any().

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() -> true.

-spec handle_request(any(), state()) -> {any(), state()}.
handle_request({document_codeaction, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}
   , <<"range">>        := RangeLSP
   , <<"context">>      := Context } = Params,
  Result = code_actions(Uri, RangeLSP, Context),
  {Result, State}.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc Result: `(Command | CodeAction)[] | null'
-spec code_actions(uri(), range(), code_action_context()) -> [map()].
code_actions(Uri, _Range, #{<<"diagnostics">> := Diagnostics}) ->
  lists:flatten([make_code_action(Uri, D) || D <- Diagnostics]).

-spec make_code_action(uri(), map()) -> [map()].
make_code_action(Uri,
  #{<<"message">> := Message, <<"range">> := Range} = D) ->
    Data = maps:get(<<"data">>, D, <<>>),
  make_code_action(
    [ {"function (.*) is unused", fun action_export_function/4}
    , {"variable '(.*)' is unused", fun action_ignore_variable/4}
    , {"variable '(.*)' is unbound", fun action_suggest_variable/4}
    , {"Module name '(.*)' does not match file name '(.*)'",
       fun action_fix_module_name/4}
    , {"Unused macro: (.*)", fun action_remove_macro/4}
    , {"function (.*) undefined", fun action_create_function/4}
    , {"Unused file: (.*)", fun action_remove_unused/4}
    ], Uri, Range, Data, Message).

-spec make_code_action([{string(), Fun}], uri(), range(), binary(), binary())
       -> [map()]
          when Fun :: fun((uri(), range(), binary(), [binary()]) -> [map()]).
make_code_action([], _Uri, _Range, _Data, _Message) ->
  [];
make_code_action([{RE, Fun}|Rest], Uri, Range, Data, Message) ->
  Actions = case re:run(Message, RE, [{capture, all_but_first, binary}]) of
              {match, Matches} ->
                Fun(Uri, Range, Data, Matches);
              nomatch ->
                []
            end,
  Actions ++ make_code_action(Rest, Uri, Range, Data, Message).



-spec action_create_function(uri(), range(), binary(), [binary()]) -> [map()].
action_create_function(Uri, _Range, _Data, [UndefinedFun]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  case els_poi:sort(els_dt_document:pois(Document)) of
    [] ->
      [];
    POIs->
      #{range := #{to := {Line, _Col}}} = lists:last(POIs),

      [FunctionName,_Arity] = string:split(UndefinedFun, "/"),
      [ make_edit_action( Uri
                          , <<"Add the undefined function ", UndefinedFun/binary>>
                          , ?CODE_ACTION_KIND_QUICKFIX
                          , <<"-spec ", FunctionName/binary, "() -> ok. \n ", FunctionName/binary,"() -> \n \t ok.">>
                          , els_protocol:range(#{from => {Line+1,1}, to => {Line+2,1}}))]
  end.


-spec action_export_function(uri(), range(), binary(), [binary()]) -> [map()].
action_export_function(Uri, _Range, _Data, [UnusedFun]) ->
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

-spec action_ignore_variable(uri(), range(), binary(), [binary()]) -> [map()].
action_ignore_variable(Uri, Range, _Data, [UnusedVariable]) ->
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

-spec action_suggest_variable(uri(), range(), binary(), [binary()]) -> [map()].
action_suggest_variable(Uri, Range, _Data, [Var]) ->
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
      VariableDistances =
        [{els_utils:jaro_distance(V, Var), V} || V <- VarsInScope, V =/= Var],
      [ make_edit_action( Uri
                        , <<"Did you mean '", V/binary, "'?">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , V
                        , els_protocol:range(VarRange))
        || {Distance, V} <- lists:reverse(lists:usort(VariableDistances)),
           Distance > 0.8];
    error ->
      []
  end.

-spec action_fix_module_name(uri(), range(), binary(), [binary()]) -> [map()].
action_fix_module_name(Uri, Range0, _Data, [ModName, FileName]) ->
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

- spec action_remove_macro(uri(), range(), binary(), [binary()]) -> [map()].
action_remove_macro(Uri, Range, _Data, [Macro]) ->
  %% Supply a quickfix to remove the unused Macro
  {ok, Document} = els_utils:lookup_document(Uri),
  POIs = els_poi:sort(els_dt_document:pois(Document, [define])),
  case ensure_range(els_range:to_poi_range(Range), Macro, POIs) of
    {ok, MacroRange} ->
      LineRange = els_range:line(MacroRange),
      [ make_edit_action( Uri
                        , <<"Remove unused macro ", Macro/binary, ".">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , <<"">>
                        , els_protocol:range(LineRange)) ];
    error ->
      []
  end.

-spec action_remove_unused(uri(), range(), binary(), [binary()]) -> [map()].
action_remove_unused(Uri, _Range0, Data, [Import]) ->
  {ok, Document} = els_utils:lookup_document(Uri),
  case els_range:inclusion_range(Data, Document) of
    {ok, UnusedRange} ->
      LineRange = els_range:line(UnusedRange),
      [ make_edit_action( Uri
                        , <<"Remove unused -include_lib(", Import/binary, ").">>
                        , ?CODE_ACTION_KIND_QUICKFIX
                        , <<>>
                        , els_protocol:range(LineRange)) ];
    error ->
      []
  end.

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
