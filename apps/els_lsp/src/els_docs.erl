%%==============================================================================
%% Extract and format Erlang documentation
%%==============================================================================
-module(els_docs).

%%==============================================================================
%% Exports
%%==============================================================================
-export([ docs/2
        , function_docs/4
        , type_docs/4
        ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-include_lib("kernel/include/eep48.hrl").
-endif.
-endif.

%%==============================================================================
%% Macro Definnitions
%%==============================================================================
-define(MAX_CLAUSES, 10).

%%==============================================================================
%% Types
%%==============================================================================
-type application_type() :: 'local' | 'remote'.

%%==============================================================================
%% Dialyer Ignores (due to upstream bug, see ERL-1262
%%==============================================================================
-dialyzer({nowarn_function, function_docs/4}).

%%==============================================================================
%% API
%%==============================================================================
-spec docs(uri(), poi()) -> [els_markup_content:doc_entry()].
docs(_Uri, #{kind := Kind, id := {M, F, A}})
  when Kind =:= application;
       Kind =:= implicit_fun ->
  function_docs('remote', M, F, A);
docs(Uri, #{kind := Kind, id := {F, A}})
  when Kind =:= application;
       Kind =:= implicit_fun;
       Kind =:= export_entry ->
  M = els_uri:module(Uri),
  function_docs('local', M, F, A);
docs(Uri, #{kind := macro, id := Name} = POI) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, DefUri, #{data := #{args := Args, value_range := ValueRange}}} ->
      NameStr = macro_signature(Name, Args),

      ValueText = get_valuetext(DefUri, ValueRange),

      Line = lists:flatten(["?", NameStr, " = ", ValueText]),
      [{code_line, Line}];
    _ ->
      []
  end;
docs(Uri, #{kind := record_expr} = POI) ->
  case els_code_navigation:goto_definition(Uri, POI) of
    {ok, DefUri, #{data := #{value_range := ValueRange}}} ->
      ValueText = get_valuetext(DefUri, ValueRange),

      [{code_line, ValueText}];
    _ ->
      []
  end;
docs(_M, #{kind := type_application, id := {M, F, A}}) ->
  type_docs('remote', M, F, A);
docs(_M, _POI) ->
  [].

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec function_docs(application_type(), atom(), atom(), non_neg_integer()) ->
        [els_markup_content:doc_entry()].
function_docs(Type, M, F, A) ->
    case eep48_docs(function, M, F, A) of
        {ok, Docs} ->
            [{text, Docs}];
        {error, not_available} ->
            L = [ [{h2, signature(Type, M, F, A)}]
                , function_clauses(M, F, A)
                , specs(M, F, A)
                , edoc(M, F, A)
                ],
            lists:append(L)
    end.

-spec type_docs(application_type(), atom(), atom(), non_neg_integer()) ->
        [els_markup_content:doc_entry()].
type_docs(Type, M, F, A) ->
    case eep48_docs(type, M, F, A) of
        {ok, Docs} ->
            [{text, Docs}];
        {error, not_available} ->
            Signature = signature(Type, M, F, A),
            Docs = lists:append(specs(M, F, A)),
            L = [ [{h2, Signature}]
                , Docs],
            lists:append(L)
    end.

-spec get_valuetext(uri(), map()) -> list().
get_valuetext(DefUri, #{from := From, to := To}) ->
  {ok, #{text := Text}} = els_utils:lookup_document(DefUri),
  els_utils:to_list(els_text:range(Text, From, To)).


-spec signature(application_type(), atom(), atom(), non_neg_integer()) ->
        string().
signature('local', _M, F, A) ->
  io_lib:format("~p/~p", [F, A]);
signature('remote', M, F, A) ->
  io_lib:format("~p:~p/~p", [M, F, A]).

%% @doc Fetch EEP-48 style Docs
%%
%% On modern systems (OTP 23+), Erlang has support for fetching documentation
%% from the chunk.
-ifdef(NATIVE_FORMAT).
-spec eep48_docs(function | type, atom(), atom(), non_neg_integer()) ->
        {ok, string()} | {error, not_available}.
eep48_docs(Type, M, F, A) ->
    Render = case Type of
             function ->
                 render;
             type ->
                 render_type
         end,
  try get_doc_chunk(M) of
    {ok, #docs_v1{ format = ?NATIVE_FORMAT
                 , module_doc = MDoc
                 } = DocChunk} when MDoc =/= none ->

          case els_eep48_docs:Render(M, F, A, DocChunk) of
              {error, _R0} ->
                  case els_eep48_docs:Render(M, F, DocChunk) of
                      {error, _R1} ->
                          {error, not_available};
                      Docs ->
                          {ok, els_utils:to_list(Docs)}
                  end;
              Docs ->
                  {ok, els_utils:to_list(Docs)}
          end;
      _R1 ->
          {error, not_available}
  catch C:E:ST ->
      %% code:get_doc/1 fails for escriptized modules, so fall back
      %% reading docs from source. See #751 for details
      Fmt = "Error fetching docs, falling back to src."
        " module=~p error=~p:~p st=~p",
      Args = [M, C, E, ST],
      ?LOG_WARNING(Fmt, Args),
      {error, not_available}
  end.

%% This function first tries to read the doc chunk from the .beam file
%% and if that fails it attempts to find the .chunk file.
-spec get_doc_chunk(M :: module()) -> {ok, term()} | error.
get_doc_chunk(M) ->
  {ok, Uri} = els_utils:find_module(M),

  SrcDir    = filename:dirname(els_utils:to_list(els_uri:path(Uri))),
  BeamFile  = filename:join([SrcDir, "..", "ebin", lists:concat([M, ".beam"])]),
  ChunkFile = filename:join([SrcDir, "..", "doc", "chunks",
                             lists:concat([M, ".chunk"])]),
  case beam_lib:chunks(BeamFile, ["Docs"]) of
    {ok, {_Mod, [{"Docs", Bin}]}} ->
        {ok, binary_to_term(Bin)};
    _ ->
      case file:read_file(ChunkFile) of
        {ok, Bin} ->
          {ok, binary_to_term(Bin)};
        _ ->
          get_edoc_chunk(M, Uri)
      end
  end.

-spec get_edoc_chunk(M :: module(), Uri :: uri()) -> {ok, term()} | error.
get_edoc_chunk(M, Uri) ->
    case {code:ensure_loaded(edoc_doclet_chunks),
          code:ensure_loaded(edoc_layout_chunks)} of
        {{module, _}, {module, _}} ->
            Path = els_uri:path(Uri),
            ok = edoc:run([els_utils:to_list(Path)],
                          [{private, true},
                           {preprocess, true},
                           {doclet, edoc_doclet_chunks},
                           {layout, edoc_layout_chunks},
                           {dir, "/tmp"}]),
            %% Should store this is a better place...
            Chunk = "/tmp/chunks/" ++ atom_to_list(M) ++ ".chunk",
            {ok, Bin} = file:read_file(Chunk),
            file:del_dir_r("/tmp/chunks/"),
            {ok, binary_to_term(Bin)};
        E ->
            ?LOG_ERROR("[edoc_chunk] load error", [E]),
            error
    end.
-else.
-spec eep48_docs(function | type, atom(), atom(), non_neg_integer()) ->
        {error, not_available}.
eep48_docs(_Type, _M, _F, _A) ->
    {error, not_available}.
-endif.

-spec specs(atom(), atom(), non_neg_integer()) ->
        [els_markup_content:doc_entry()].
specs(M, F, A) ->
  case els_dt_signatures:lookup({M, F, A}) of
    {ok, [#{spec := Spec}]} ->
      [ {code_line, els_utils:to_list(Spec)} ];
    {ok, []} ->
      []
  end.

-spec function_clauses(atom(), atom(), non_neg_integer()) ->
        [els_markup_content:doc_entry()].
function_clauses(_Module, _Function, 0) ->
  [];
function_clauses(Module, Function, Arity) ->
  case els_utils:find_module(Module) of
    {ok, Uri} ->
      {ok, Doc} = els_utils:lookup_document(Uri),
      ClausesPOIs = els_dt_document:pois(Doc, [function_clause]),
      Lines = [{code_block_line, atom_to_list(F) ++ els_utils:to_list(Data)}
                || #{id := {F, A, _}, data := Data} <- ClausesPOIs,
                   F =:= Function, A =:= Arity],
      lists:append([ [{code_block_begin, "erlang"}]
                   , truncate_lines(Lines)
                   , [{code_block_end, "erlang"}]
                   ]);
    {error, _Reason} ->
      []
  end.

-spec truncate_lines([els_markup_content:doc_entry()]) ->
        [els_markup_content:doc_entry()].
truncate_lines(Lines) when length(Lines) =< ?MAX_CLAUSES ->
  Lines;
truncate_lines(Lines0) ->
  Lines = lists:sublist(Lines0, ?MAX_CLAUSES),
  lists:append(Lines, [{code_block_line, "[...]"}]).

-spec edoc(atom(), atom(), non_neg_integer()) ->
          [els_markup_content:doc_entry()].
edoc(M, F, A) ->
  try
    {ok, Uri} = els_utils:find_module(M),
    Path      = els_uri:path(Uri),
    {M, EDoc} = edoc:get_doc( els_utils:to_list(Path)
                             , [{private, true},
                                {preprocess, true}]
                             ),
    Internal  = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
    %% TODO: Something is weird with the docsh specs.
    %%       For now, let's avoid the Dialyzer warnings.
    Docs = erlang:apply(docsh_docs_v1, from_internal, [Internal]),
    Res  = erlang:apply(docsh_docs_v1, lookup, [ Docs
                                               , {M, F, A}
                                               , [doc, spec]]),
    {ok, [{{function, F, A}, _Anno, _Signature, Desc, _Metadata}|_]} = Res,
    format_edoc(Desc)
  catch C:E:ST ->
      ?LOG_ERROR("[hover] Error fetching edoc [error=~p]",
                 [{M, F, A, C, E, ST}]),
      []
  end.

-spec format_edoc(none | map()) -> [els_markup_content:doc_entry()].
format_edoc(none) ->
  [];
format_edoc(Desc) when is_map(Desc) ->
  Lang         = <<"en">>,
  Doc          = maps:get(Lang, Desc, <<>>),
  FormattedDoc = els_utils:to_list(docsh_edoc:format_edoc(Doc, #{})),
  [{text, FormattedDoc}].

-spec macro_signature(poi_id(), [{integer(), string()}]) -> unicode:charlist().
macro_signature({Name, _Arity}, Args) ->
  [atom_to_list(Name), "(", lists:join(", ", [A || {_N, A} <- Args]), ")"];
macro_signature(Name, none) ->
  atom_to_list(Name).
