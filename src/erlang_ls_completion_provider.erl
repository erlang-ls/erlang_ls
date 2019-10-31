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
  Token = erlang_ls_text:last_token(Prefix),
  try erl_syntax:type(Token) of
    atom ->
      Module = erl_syntax:atom_value(Token),
      exported_functions(Module);
    _ -> null
  catch error:{badarg, _} ->
      null
  end;
find_completion( Prefix
               , ?COMPLETION_TRIGGER_KIND_INVOKED
               , #{document := Document}
               ) ->
  case lists:reverse(erlang_ls_text:tokens(Prefix)) of
    [{atom, _, _}, {':', _}, {atom, _, Module} | _] ->
      exported_functions(Module);
    _ ->
      functions(Document, function)
        ++ modules(Prefix)
        ++ variables(Document)
  end;
find_completion(_Prefix, _TriggerKind, _Opts) ->
  null.

-spec modules(binary()) -> [map()].
modules(_Prefix) ->
  [ #{ label            => atom_to_binary(K, utf8)
     , kind             => ?COMPLETION_ITEM_KIND_MODULE
     , insertTextFormat => ?INSERT_TEXT_FORMAT_PLAIN_TEXT
     } || K <- erlang_ls_db:keys(completion_index)
  ].

-spec functions(erlang_ls_document:document(), poi_kind()) -> [map()].
functions(Document, POIKind) ->
  POIs = erlang_ls_document:points_of_interest(Document, [POIKind]),
  [ #{ label            => list_to_binary(io_lib:format("~p/~p", [F, A]))
     , kind             => ?COMPLETION_ITEM_KIND_FUNCTION
     , insertText       => snippet_function_call(F, A)
     , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
     }
    || #{data := {F, A}} <- POIs
  ].

-spec exported_functions(module()) -> [map()] | null.
exported_functions(Module) ->
  case erlang_ls_utils:find_module(Module) of
    {ok, Uri} ->
      {ok, Document} = erlang_ls_db:find(documents, Uri),
      functions(Document, exports_entry);
    {error, _Error} ->
      null
  end.

-spec snippet_function_call(atom(), non_neg_integer()) -> binary().
snippet_function_call(Function, Arity) ->
  Args = ["$" ++ integer_to_list(N) || N <- lists:seq(1, Arity)],
  Format = "~p(" ++ string:join(Args, ", ") ++ ")",
  list_to_binary(io_lib:format(Format, [Function])).

-spec variables(erlang_ls_document:document()) -> [map()].
variables(Document) ->
  POIs = erlang_ls_document:points_of_interest(Document, [variable]),
  Vars = [ #{ label => atom_to_binary(Name, utf8)
            , kind  => ?COMPLETION_ITEM_KIND_VARIABLE
            }
           || #{data := Name} <- POIs
         ],
  lists:usort(Vars).
