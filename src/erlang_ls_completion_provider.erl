-module(erlang_ls_completion_provider).

-behaviour(erlang_ls_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  #{}.

-spec teardown() -> ok.
teardown() ->
  ok.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({completion, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  Text = erlang_ls_document:text(Document),
  case maps:find(<<"context">>, Params) of
    {ok, Context} ->
      TriggerKind = maps:get(<<"triggerKind">>, Context),
      TriggerCharacter = maps:get(<<"triggerCharacter">>, Context, <<>>),
      %% We subtract 1 to strip the character that triggered the
      %% completion from the string.
      Length = case Character > 0 of true -> 1; false -> 0 end,
      Prefix = case TriggerKind of
                 ?COMPLETION_TRIGGER_KIND_CHARACTER ->
                   erlang_ls_text:line(Text, Line, Character - Length);
                 ?COMPLETION_TRIGGER_KIND_INVOKED ->
                   erlang_ls_text:line(Text, Line, Character)
               end,
      Opts   = #{ trigger  => TriggerCharacter
                , document => Document
                },
      {find_completion(Prefix, TriggerKind, Opts), State};
    error ->
      {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_completion(binary(), integer(), map()) -> any().
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<":">>}
               ) ->
  case erlang_ls_text:last_token(Prefix) of
    {atom, _, Module} -> exported_functions(Module);
    _ -> null
  end;
find_completion( _Prefix
               , ?COMPLETION_TRIGGER_KIND_CHARACTER
               , #{trigger := <<"?">>, document := Document}
               ) ->
  macros(Document);
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_INVOKED
               , #{document := Document}
               ) ->
  case lists:reverse(erlang_ls_text:tokens(Prefix)) of
    %% Check for "[...] atom:atom"
    [{atom, _, _}, {':', _}, {atom, _, Module} | _] ->
      exported_functions(Module);
    %% Check for "[...] ?anything"
    [_, {'?', _} | _] ->
      macros(Document);
    %% Check for "[...] Variable"
    [{var, _, _} | _] ->
      variables(Document);
    %% Check for "[...] atom"
    [{atom, _, _} | _] ->
      modules(Prefix) ++ functions(Document, false);
    _ ->
      modules(Prefix) ++ functions(Document, false) ++ variables(Document)
  end;
find_completion(_Prefix, _TriggerKind, _Opts) ->
  null.

%%==============================================================================
%% Modules

-spec modules(binary()) -> [map()].
modules(_Prefix) ->
  [ #{ label            => atom_to_binary(K, utf8)
     , kind             => ?COMPLETION_ITEM_KIND_MODULE
     , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
     } || K <- erlang_ls_db:keys(completion_index)
  ].

%%==============================================================================
%% Functions

-spec functions(erlang_ls_document:document(), boolean()) -> [map()].
functions(Document, _OnlyExported = false) ->
  POIs = erlang_ls_document:points_of_interest(Document, [function]),
  [completion_item_function(POI) || POI <- POIs];
functions(Document, _OnlyExported = true) ->
  Exports   = erlang_ls_document:points_of_interest(Document, [exports_entry]),
  Functions = erlang_ls_document:points_of_interest(Document, [function]),
  ExportsFA = [FA || #{data := FA} <- Exports],
  [ completion_item_function(POI)
    || #{data := FA} = POI <- Functions, lists:member(FA, ExportsFA)
  ].

-spec completion_item_function(poi()) -> map().
completion_item_function(#{data := {F, A}, tree := Tree}) ->
  #{ label            => list_to_binary(io_lib:format("~p/~p", [F, A]))
   , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
   , insertText       => snippet_function_call(F, function_args(Tree, A))
   , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
   }.

-spec exported_functions(module()) -> [map()] | null.
exported_functions(Module) ->
  case erlang_ls_utils:find_module(Module) of
    {ok, Uri} ->
      {ok, Document} = erlang_ls_db:find(documents, Uri),
      functions(Document, true);
    {error, _Error} ->
      null
  end.

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

-spec snippet_function_call(atom(), [{integer(), string()}]) -> binary().
snippet_function_call(Function, Args0) ->
  Args    = [ ["${", integer_to_list(N), ":", A, "}"]
              || {N, A} <- Args0
            ],
  Snippet = [atom_to_list(Function), "(", string:join(Args, ", "), ")"],
  iolist_to_binary(Snippet).

%%==============================================================================
%% Variables

-spec variables(erlang_ls_document:document()) -> [map()].
variables(Document) ->
  POIs = erlang_ls_document:points_of_interest(Document, [variable]),
  Vars = [ #{ label => atom_to_binary(Name, utf8)
            , kind  => ?COMPLETION_ITEM_KIND_VARIABLE
            }
           || #{data := Name} <- POIs
         ],
  lists:usort(Vars).

%%==============================================================================
%% Macros

-spec macros(erlang_ls_document:document()) -> [map()].
macros(Document) ->
  lists:flatten([local_macros(Document), included_macros(Document)]).

-spec local_macros(erlang_ls_document:document()) -> [map()].
local_macros(Document) ->
  POIs   = erlang_ls_document:points_of_interest(Document, [define]),
   [ #{ label => atom_to_binary(Name, utf8)
      , kind  => ?COMPLETION_ITEM_KIND_CONSTANT
      }
     || #{data := Name} <- POIs
   ].

-spec included_macros(erlang_ls_document:document()) -> [[map()]].
included_macros(Document) ->
  Kinds = [include, include_lib],
  POIs  = erlang_ls_document:points_of_interest(Document, Kinds),
  [ include_file_macros(Kind, Name) || #{kind := Kind, data := Name} <- POIs].

-spec include_file_macros('include' | 'include_lib', string()) -> [map()].
include_file_macros(Kind, Name) ->
  Filename = erlang_ls_utils:include_filename(Kind, Name),
  M = list_to_atom(Filename),
  case erlang_ls_utils:find_module(M) of
    {ok, Uri} ->
      {ok, IncludeDocument} = erlang_ls_db:find(documents, Uri),
      local_macros(IncludeDocument);
    {error, _} ->
      []
  end.
