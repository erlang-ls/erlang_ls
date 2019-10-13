-module(erlang_ls_definition_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  #{}.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({definition, Params}, State) ->
  Position     = maps:get(<<"position">>    , Params),
  Line         = maps:get(<<"line">>        , Position),
  Character    = maps:get(<<"character">>   , Position),
  TextDocument = maps:get(<<"textDocument">>, Params),
  Uri          = maps:get(<<"uri">>         , TextDocument),
  {ok, Buffer} = erlang_ls_db:find(erlang_ls_files, Uri),
  case erlang_ls_buffer:get_element_at_pos(Buffer, Line + 1, Character + 1) of
    [POI|_] ->
      Filename = erlang_ls_uri:path(Uri),
      case erlang_ls_code_navigation:goto_definition(Filename, POI) of
        {error, _Error} ->
          {null, State};
        {ok, FullName, Range} ->
          { #{ uri => erlang_ls_uri:uri(FullName)
             , range => erlang_ls_protocol:range(Range)
             }
          , State
          }
      end;
    [] ->
      {null, State}
  end.

-spec teardown() -> ok.
teardown() ->
  ok.
