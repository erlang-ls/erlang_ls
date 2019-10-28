-module(erlang_ls_hover_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  ok.

-spec teardown() -> ok.
teardown() ->
  ok.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({hover, Params}, State) ->
  #{ <<"position">>     := #{ <<"line">>      := Line
                            , <<"character">> := Character
                            }
   , <<"textDocument">> := #{<<"uri">> := Uri}
   } = Params,
  case documentation(Uri, Line, Character) of
    <<>> ->
      {null, State};
    Doc ->
      {#{contents => Doc}, State}
  end.

-spec documentation(uri(), non_neg_integer(), non_neg_integer()) -> binary().
documentation(Uri, Line, Character) ->
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  case erlang_ls_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI|_] -> documentation(POI);
    []      -> <<>>
  end.

-spec documentation(poi()) -> binary().
documentation(#{kind := application, data := {M, F, A}}) ->
  case {specs(M, F, A), edoc(M, F, A)} of
    {<<>>, <<>>} ->
      <<>>;
    {Specs, Edoc} ->
      #{ kind => <<"markdown">>
       , value => <<Specs/binary, "\n\n", Edoc/binary>>
       }
  end;
documentation(_POI) ->
  <<>>.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec specs(atom(), atom(), non_neg_integer()) -> binary().
specs(M, F, A) ->
  case erlang_ls_db:find(specs_index, {M, F, A}) of
    {ok, Doc}          -> list_to_binary(erl_prettypr:format(Doc));
    {error, not_found} -> <<>>
  end.

-spec edoc(atom(), atom(), non_neg_integer()) -> binary().
edoc(M, F, A) ->
  case erlang_ls_utils:find_module(M) of
    {ok, Uri} ->
      Path      = erlang_ls_uri:path(Uri),
      {M, EDoc} = edoc:get_doc(binary_to_list(Path), [{private, true}]),
      Internal  = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
      %% TODO: Something is weird with the docsh specs.
      %%       For now, let's avoid the Dialyzer warnings.
      Docs = erlang:apply(docsh_docs_v1, from_internal, [Internal]),
      Res  = erlang:apply(docsh_docs_v1, lookup, [ Docs
                                                 , {M, F, A}
                                                 , [doc, spec]]),
      case Res of
        {ok, [{{function, F, A}, _Anno, Signature, Desc, _Metadata}|_]} ->
          format(Signature, Desc);
        {not_found, Message} ->
          Message;
        Error ->
          lager:error("[hover] Error finding edoc [error=~p]", [Error]),
          <<>>
      end;
    {error, Error} ->
      lager:error("[hover] Error finding module [error=~p]", [Error]),
      <<>>
  end.

-spec format(binary(), none | map()) -> binary().
format(_Signature, none) ->
  <<>>;
format(Signature, Desc) when is_map(Desc) ->
  Lang         = <<"en">>,
  Doc          = maps:get(Lang, Desc, <<>>),
  FormattedDoc = list_to_binary(docsh_edoc:format_edoc(Doc, #{})),
  <<"# ", Signature/binary, "\n", FormattedDoc/binary>>.
