-module(els_hover_provider).

-behaviour(els_provider).

-export([ handle_request/2
        , is_enabled/0
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% els_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec handle_request(any(), els_provider:state()) ->
  {any(), els_provider:state()}.
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
  {ok, Document} = els_utils:lookup_document(Uri),
  case els_dt_document:get_element_at_pos(Document, Line + 1, Character + 1)
  of
    [POI|_] -> documentation(els_uri:module(Uri), POI);
    []      -> <<>>
  end.

%% @doc docs for documentation
-spec documentation(atom(), poi()) -> binary().
documentation(_M, #{kind := application, id := {M, F, A}}) ->
  get_docs(M, F, A);
documentation(M, #{kind := application, id := {F, A}}) ->
  get_docs(M, F, A);
documentation(M, #{kind := export_entry, id := {F, A}}) ->
  get_docs(M, F, A);
documentation(_M, _POI) ->
  <<>>.

%% @doc get the docs
-spec get_docs(atom(), atom(), byte()) -> binary().
get_docs(M, F, A) ->
  case {specs(M, F, A), edoc(M, F, A)} of
    {<<>>, <<>>} ->
      <<>>;
    {Specs, Edoc} ->
      ContentKind = content_kind(),
      FormattedSpecs = format_code(ContentKind, Specs),
      #{ kind  => ContentKind
       , value => << FormattedSpecs/binary, "\n", Edoc/binary>>
       }
  end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec specs(atom(), atom(), non_neg_integer()) -> binary().
specs(M, F, A) ->
  case els_dt_signatures:lookup({M, F, A}) of
    {ok, [#{tree := Tree}]} ->
      Specs = erl_prettypr:format(Tree),
      unicode:characters_to_binary(Specs);
    {ok, []} ->
      <<>>
  end.

-spec format_code(markup_kind(), binary()) -> binary().
format_code(plaintext, Code) ->
  Code;
format_code(markdown, Code) ->
  <<"```erlang\n", Code/binary, "\n```\n">>.

-spec edoc(atom(), atom(), non_neg_integer()) -> binary().
edoc(M, F, A) ->
  try
    {ok, Uri} = els_utils:find_module(M),
    Path      = els_uri:path(Uri),
    {M, EDoc} = edoc:get_doc(binary_to_list(Path), [{private, true}]),
    Internal  = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
    %% TODO: Something is weird with the docsh specs.
    %%       For now, let's avoid the Dialyzer warnings.
    Docs = erlang:apply(docsh_docs_v1, from_internal, [Internal]),
    Res  = erlang:apply(docsh_docs_v1, lookup, [ Docs
                                               , {M, F, A}
                                               , [doc, spec]]),
    {ok, [{{function, F, A}, _Anno, Signature, Desc, _Metadata}|_]} = Res,
    format(Signature, Desc)
  catch C:E ->
      lager:error("[hover] Error fetching edoc [error=~p]", [{C, E}]),
      <<>>
  end.

-spec format(binary(), none | map()) -> binary().
format(_Signature, none) ->
  <<>>;
format(Signature, Desc) when is_map(Desc) ->
  Lang         = <<"en">>,
  Doc          = maps:get(Lang, Desc, <<>>),
  FormattedDoc = list_to_binary(docsh_edoc:format_edoc(Doc, #{})),
  <<"### ", Signature/binary, "\n", FormattedDoc/binary>>.

-spec content_kind() -> markup_kind().
content_kind() ->
  ContentFormat =
    case els_config:get(capabilities) of
      #{<<"textDocument">> := #{<<"hover">> := #{<<"contentFormat">> := X}}} ->
        X;
      _ ->
        []
    end,
  case lists:member(atom_to_binary(?MARKDOWN, utf8), ContentFormat) of
    true  -> ?MARKDOWN;
    false -> ?PLAINTEXT
  end.
