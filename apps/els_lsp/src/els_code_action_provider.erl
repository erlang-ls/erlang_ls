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
make_code_action(Uri, #{<<"message">> := Message, <<"range">> := Range}) ->
  make_code_action(
    [ {"function (.*) is unused", fun action_export_function/3}
    , {"variable '(.*)' is unused", fun action_ignore_variable/3}
    , {"variable '(.*)' is unbound", fun action_suggest_variable/3}
    , {"Module name '(.*)' does not match file name '(.*)'",
       fun action_fix_module_name/3}
    , {"Unused macro: (.*)", fun action_remove_macro/3}
    ], Uri, Range, Message).

-spec make_code_action([{string(), Fun}], uri(), range(), binary()) -> [map()]
          when Fun :: fun((uri(), range(), [binary()]) -> [map()]).
make_code_action([], _Uri, _Range, _Message) ->
  [];
make_code_action([{RE, Fun}|Rest], Uri, Range, Message) ->
  Actions = case re:run(Message, RE, [{capture, all_but_first, binary}]) of
              {match, Matches} ->
                Fun(Uri, Range, Matches);
              nomatch ->
                []
            end,
  Actions ++ make_code_action(Rest, Uri, Range, Message).

-spec action_export_function(uri(), range(), [binary()]) -> [map()].
action_export_function(Uri, _Range, [UnusedFun]) ->
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

-spec action_ignore_variable(uri(), range(), [binary()]) -> [map()].
action_ignore_variable(Uri, Range, [UnusedVariable]) ->
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

-spec action_suggest_variable(uri(), range(), [binary()]) -> [map()].
action_suggest_variable(Uri, Range, [Var]) ->
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

-spec action_fix_module_name(uri(), range(), [binary()]) -> [map()].
action_fix_module_name(Uri, Range0, [ModName, FileName]) ->
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

- spec action_remove_macro(uri(), range(), [binary()]) -> [map()].
action_remove_macro(Uri, Range, [Macro]) ->
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
