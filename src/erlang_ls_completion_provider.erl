-module(erlang_ls_completion_provider).

-behaviour(erlang_ls_provider).

-include("erlang_ls.hrl").

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

-define(TRIGGER_INVOKED,                    1).
-define(TRIGGER_CHARACTER,                  2).
-define(TRIGGER_FOR_INCOMPLETE_COMPLETIONS, 3).

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
   , <<"textDocument">> := #{<<"uri">> := Uri
                            }
   } = Params,
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  case maps:find(<<"context">>, Params) of
    {ok, Context} ->
      TriggerKind = maps:get(<<"triggerKind">>, Context),
      TriggerCharacter = maps:get(<<"triggerCharacter">>, Context, undefined),
      TextLine = erlang_ls_document:text_line(Document, Line),
      Prefix = binary:part(TextLine, {0, Character}),
      {find_completion(Prefix, TriggerKind, TriggerCharacter), State};
    error ->
      {null, State}
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================

-spec find_completion(binary(), binary(), binary()) ->
   any().
find_completion(Prefix, ?TRIGGER_CHARACTER, <<":">>) ->
  {ok, Tokens, _} = erl_scan:string(binary_to_list(Prefix)),
  [{':', _}, Token | _] = lists:reverse(Tokens),
  case Token of
    {atom, _, Module} ->
      case erlang_ls_completion_index:find(Module) of
        {ok, Uri} ->
          {ok, Document} = erlang_ls_db:find(documents, Uri),
          POIs = erlang_ls_document:points_of_interest(Document),
          [ #{ label            => list_to_binary(io_lib:format("~p/~p", [F, A]))
             , insertText       => snippet_function_call(F, A)
             , insertTextFormat => ?INSERT_TEXT_FORMAT_SNIPPET
             }
            || #{info := {exports_entry, {F, A}}} <- POIs
          ];
        not_found ->
          null
      end;
    _ -> null
  end;
find_completion(_Prefix, _TriggerKind, _TriggerCharacter) ->
  null.

-spec snippet_function_call(atom(), non_neg_integer()) -> binary().
snippet_function_call(Function, Arity) ->
  Args = ["$" ++ integer_to_list(N) || N <- lists:seq(1, Arity)],
  Format = "~p(" ++ string:join(Args, ", ") ++ ")",
  list_to_binary(io_lib:format(Format, [Function])).
