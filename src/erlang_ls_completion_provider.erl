-module(erlang_ls_completion_provider).

-behaviour(erlang_ls_provider).

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
  Pattern = <<"\s+(?<MODULE>.*):">>,
  Options = [{capture, ['MODULE'], binary}],
  case re:run(Prefix, Pattern, Options) of
    {match, [Module]} ->
      case erlang_ls_completion_index:find(binary_to_atom(Module, utf8)) of
        {ok, Uri} ->
          {ok, Document} = erlang_ls_db:find(documents, Uri),
          POIs = erlang_ls_document:points_of_interest(Document),
          [#{ label => list_to_binary(io_lib:format("~p/~p", [F, A]))
            } || #{info := {exports_entry, {F, A}}} <- POIs];
        not_found ->
          null
      end;
    nomatch ->
      null
  end;
find_completion(_Prefix, _TriggerKind, _TriggerCharacter) ->
  null.
