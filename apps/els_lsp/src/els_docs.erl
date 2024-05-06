%%==============================================================================
%% Extract and format Erlang documentation
%%==============================================================================
-module(els_docs).

%%==============================================================================
%% Exports
%%==============================================================================
-export([
    docs/2,
    function_docs/4,
    type_docs/4
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
-include_lib("kernel/include/eep48.hrl").
-export([eep48_docs/4]).
-type docs_v1() :: #docs_v1{}.
-endif.
-endif.

%%==============================================================================
%% Macro Definitions
%%==============================================================================
-define(MAX_CLAUSES, 10).

%%==============================================================================
%% Types
%%==============================================================================
-type application_type() :: 'local' | 'remote'.

%%==============================================================================
%% Dialyzer Ignores (due to upstream bug, see ERL-1262
%%==============================================================================
-dialyzer({nowarn_function, function_docs/4}).

%%==============================================================================
%% API
%%==============================================================================
-spec docs(uri(), els_poi:poi()) -> [els_markup_content:doc_entry()].
docs(_Uri, #{kind := Kind, id := {M, F, A}}) when
    Kind =:= application;
    Kind =:= implicit_fun
->
    function_docs('remote', M, F, A);
docs(Uri, #{kind := Kind, id := {F, A}}) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= export_entry;
    Kind =:= spec
->
    M = els_uri:module(Uri),
    function_docs('local', M, F, A);
docs(Uri, #{kind := function_clause, id := {F, A, _Index}}) ->
    M = els_uri:module(Uri),
    function_docs('local', M, F, A);
docs(Uri, #{kind := macro, id := Name} = POI) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, [{DefUri, #{data := #{args := Args, value_range := ValueRange}}}]} when
            is_list(Args); is_atom(Name)
        ->
            NameStr = macro_signature(Name, Args),

            ValueText = get_valuetext(DefUri, ValueRange),

            Line = lists:flatten(["?", NameStr, " = ", ValueText]),
            [{code_line, Line}];
        _ ->
            []
    end;
docs(Uri, #{kind := record_expr} = POI) ->
    case els_code_navigation:goto_definition(Uri, POI) of
        {ok, [{DefUri, #{data := #{value_range := ValueRange}}}]} ->
            ValueText = get_valuetext(DefUri, ValueRange),

            [{code_line, ValueText}];
        _ ->
            []
    end;
docs(_M, #{kind := Kind, id := {M, F, A}}) when
    Kind =:= type_application;
    Kind =:= type_definition
->
    type_docs('remote', M, F, A);
docs(Uri, #{kind := Kind, id := {F, A}}) when
    Kind =:= type_application;
    Kind =:= type_definition
->
    type_docs('local', els_uri:module(Uri), F, A);
docs(_M, _POI) ->
    [].

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec function_docs(application_type(), atom(), atom(), non_neg_integer()) ->
    [els_markup_content:doc_entry()].
function_docs(Type, M, F, A) ->
    case edoc_parse_enabled() of
        true ->
            function_docs(Type, M, F, A, els_config:get(docs_memo));
        false ->
            Sig = {h2, signature(Type, M, F, A)},
            L = [
                function_clauses(M, F, A),
                specs(M, F, A)
            ],
            case lists:append(L) of
                [] ->
                    [Sig];
                Docs ->
                    [Sig, {text, "---"} | Docs]
            end
    end.

-spec function_docs(application_type(), atom(), atom(), non_neg_integer(), boolean()) ->
    [els_markup_content:doc_entry()].
function_docs(Type, M, F, A, true = _DocsMemo) ->
    MFACT = {M, F, A, Type, function},
    case els_docs_memo:lookup(MFACT) of
        {ok, [#{entries := Entries}]} ->
            Entries;
        {ok, []} ->
            Entries = function_docs(Type, M, F, A, false),
            ok = els_docs_memo:insert(#{mfact => MFACT, entries => Entries}),
            Entries
    end;
function_docs(Type, M, F, A, false = _DocsMemo) ->
    %% call via ?MODULE to enable mocking in tests
    case ?MODULE:eep48_docs(function, M, F, A) of
        {ok, Docs} ->
            [{text, Docs}];
        {error, not_available} ->
            %% We cannot fetch the EEP-48 style docs, so instead we create
            %% something similar using the tools we have.
            Sig = {h2, signature(Type, M, F, A)},
            L = [
                function_clauses(M, F, A),
                specs(M, F, A),
                edoc(M, F, A)
            ],
            case lists:append(L) of
                [] ->
                    [Sig];
                Docs ->
                    [Sig, {text, "---"} | Docs]
            end
    end.

-spec type_docs(application_type(), atom(), atom(), non_neg_integer()) ->
    [els_markup_content:doc_entry()].
type_docs(Type, M, F, A) ->
    case edoc_parse_enabled() of
        true ->
            type_docs(Type, M, F, A, els_config:get(docs_memo));
        false ->
            type(M, F, A)
    end.

-spec type_docs(application_type(), atom(), atom(), non_neg_integer(), boolean()) ->
    [els_markup_content:doc_entry()].
type_docs(Type, M, F, A, true = _DocsMemo) ->
    MFACT = {M, F, A, Type, type},
    case els_docs_memo:lookup(MFACT) of
        {ok, [#{entries := Entries}]} ->
            Entries;
        {ok, []} ->
            Entries = type_docs(Type, M, F, A, false),
            ok = els_docs_memo:insert(#{mfact => MFACT, entries => Entries}),
            Entries
    end;
type_docs(_Type, M, F, A, false = _DocsMemo) ->
    %% call via ?MODULE to enable mocking in tests
    case ?MODULE:eep48_docs(type, M, F, A) of
        {ok, Docs} ->
            [{text, Docs}];
        {error, not_available} ->
            type(M, F, A)
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
%% On OTP 23+ systems, Erlang has support for fetching documentation
%% from the code or from a .chunk file. This function tries to fetch
%% and render the documentation using EEP-48 docs if available.
%%
%% If it is not available it tries to create the EEP-48 style docs
%% using edoc.
-ifdef(NATIVE_FORMAT).
-spec eep48_docs(function | type, atom(), atom(), non_neg_integer()) ->
    {ok, string()} | {error, not_available}.
eep48_docs(Type, M, F, A) ->
    Render =
        case Type of
            function ->
                render;
            type ->
                render_type
        end,
    GL = setup_group_leader_proxy(),
    try get_doc_chunk(M) of
        {ok,
            #docs_v1{
                format = ?NATIVE_FORMAT,
                module_doc = MDoc
            } = DocChunk} when MDoc =/= hidden ->
            flush_group_leader_proxy(GL),

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
            ?LOG_DEBUG(#{error => _R1}),
            {error, not_available}
    catch
        C:E:ST ->
            %% code:get_doc/1 fails for escriptized modules, so fall back
            %% reading docs from source. See #751 for details
            IO = flush_group_leader_proxy(GL),
            ?LOG_DEBUG(#{
                slogan => "Error fetching docs, falling back to src.",
                module => M,
                error => {C, E},
                st => ST,
                io => IO
            }),
            {error, not_available}
    end.

%% This function first tries to read the doc chunk from the .beam file
%% and if that fails it attempts to find the .chunk file.
-spec get_doc_chunk(M :: module()) -> {ok, term()} | error.
get_doc_chunk(M) ->
    {ok, Uris} = els_utils:find_modules(M),

    %% We loop through all modules in search path to see if any
    %% of them have any docs. In the normal case there should only
    %% be one module in the path, but for example when developing
    %% Erlang/OTP there will be two "lists" modules, and we want to
    %% fetch the docs from any version that has docs built.

    case
        lists:foldl(
            fun
                (Uri, undefined) ->
                    get_doc_chunk(M, Uri);
                (_Uri, Chunk) ->
                    Chunk
            end,
            undefined,
            Uris
        )
    of
        undefined ->
            get_edoc_chunk(M, hd(Uris));
        Chunk ->
            {ok, Chunk}
    end.

-spec get_doc_chunk(module(), uri()) -> docs_v1() | undefined.
get_doc_chunk(M, Uri) ->
    SrcDir = filename:dirname(els_utils:to_list(els_uri:path(Uri))),
    BeamFile = filename:join([
        SrcDir,
        "..",
        "ebin",
        lists:concat([M, ".beam"])
    ]),
    ChunkFile = filename:join([
        SrcDir,
        "..",
        "doc",
        "chunks",
        lists:concat([M, ".chunk"])
    ]),
    case beam_lib:chunks(BeamFile, ["Docs"]) of
        {ok, {_Mod, [{"Docs", Bin}]}} ->
            binary_to_term(Bin);
        _ ->
            case file:read_file(ChunkFile) of
                {ok, Bin} ->
                    binary_to_term(Bin);
                _ ->
                    undefined
            end
    end.

-spec get_edoc_chunk(M :: module(), Uri :: uri()) -> {ok, term()} | error.
get_edoc_chunk(M, Uri) ->
    %% edoc in Erlang/OTP 24 and later can create doc chunks for edoc
    case {code:ensure_loaded(edoc_doclet_chunks), code:ensure_loaded(edoc_layout_chunks)} of
        {{module, _}, {module, _}} ->
            case edoc_run(Uri) of
                ok ->
                    {ok, Bin} = file:read_file(chunk_file_path(M)),
                    {ok, binary_to_term(Bin)};
                error ->
                    error
            end;
        E ->
            ?LOG_DEBUG("[edoc_chunk] load error", [E]),
            error
    end.

-spec chunk_file_path(module()) -> file:filename_all().
chunk_file_path(M) ->
    Dir = erlang_ls:cache_root(),
    filename:join([Dir, "chunks", atom_to_list(M) ++ ".chunk"]).

-spec is_chunk_file_up_to_date(binary(), module()) -> boolean().
is_chunk_file_up_to_date(Path, Module) ->
    ChunkPath = chunk_file_path(Module),
    filelib:is_file(ChunkPath) andalso
        filelib:last_modified(ChunkPath) > filelib:last_modified(Path).

-spec edoc_run(uri()) -> ok | error.
edoc_run(Uri) ->
    Ref = make_ref(),
    Module = els_uri:module(Uri),
    Path = els_uri:path(Uri),
    Opts = [
        {doclet, edoc_doclet_chunks},
        {layout, edoc_layout_chunks},
        {dir, erlang_ls:cache_root()}
        | edoc_options()
    ],
    Parent = self(),
    case is_chunk_file_up_to_date(Path, Module) of
        true ->
            ?LOG_DEBUG("Chunk file is up to date!"),
            ok;
        false ->
            %% Run job to generate chunk file
            %% This can be slow, run it in a spawned process so
            %% we can timeout
            spawn_link(
                fun() ->
                    Name = list_to_atom(lists:concat(['docs_', Module])),
                    try
                        %% Use register to ensure we only run one of these
                        %% processes at the same time.
                        true = register(Name, self()),
                        ?LOG_DEBUG("Generating doc chunks for ~s.", [Module]),
                        Res = edoc:run([els_utils:to_list(Path)], Opts),
                        ?LOG_DEBUG("Done generating doc chunks for ~s.", [Module]),
                        Parent ! {Ref, Res}
                    catch
                        _:Err:St ->
                            ?LOG_INFO(
                                "Generating do chunks for ~s failed: ~p\n~p",
                                [Module, Err, St]
                            ),
                            %% Respond to parent with error
                            Parent ! {Ref, error}
                    end
                end
            ),
            receive
                {Ref, Res} ->
                    Res
            after 1000 ->
                %% This took too long, return and let job continue
                %% running in background in order to let it generate
                %% a chunk file
                error
            end
    end.

-else.
-dialyzer({no_match, function_docs/5}).
-dialyzer({no_match, type_docs/5}).
-spec eep48_docs(function | type, atom(), atom(), non_neg_integer()) ->
    {error, not_available}.
eep48_docs(_Type, _M, _F, _A) ->
    {error, not_available}.
-endif.

-spec edoc_options() ->
    [
        {'includes' | 'macros', [any()]}
        | {'preprocess', 'true'}
    ].
edoc_options() ->
    [
        {preprocess, true},
        {macros, [{N, V} || {'d', N, V} <- els_compiler_diagnostics:macro_options()]},
        {includes, [I || {i, I} <- els_compiler_diagnostics:include_options()]}
    ].

-spec specs(atom(), atom(), non_neg_integer()) ->
    [els_markup_content:doc_entry()].
specs(M, F, A) ->
    case els_dt_signatures:lookup({M, F, A}) of
        {ok, [#{spec := Spec}]} ->
            [{code_line, els_utils:to_list(Spec)}];
        {ok, []} ->
            []
    end.

-spec type(module(), atom(), arity()) ->
    [els_markup_content:doc_entry()].
type(M, T, A) ->
    case els_utils:find_module(M) of
        {ok, Uri} ->
            {ok, Document} = els_utils:lookup_document(Uri),
            ExportedTypes = els_dt_document:pois(Document, [type_definition]),
            case
                lists:search(
                    fun(#{id := Id}) ->
                        Id =:= {T, A}
                    end,
                    ExportedTypes
                )
            of
                {value, #{range := Range}} ->
                    [{code_line, get_valuetext(Uri, Range)}];
                false ->
                    []
            end;
        _ ->
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
            Lines = [
                {code_block_line, atom_to_list(F) ++ els_utils:to_list(Data)}
             || #{id := {F, A, _}, data := Data} <- ClausesPOIs,
                F =:= Function,
                A =:= Arity
            ],
            lists:append([
                [{code_block_begin, "erlang"}],
                truncate_lines(Lines),
                [{code_block_end, "erlang"}]
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
    case els_utils:find_module(M) of
        {ok, Uri} ->
            GL = setup_group_leader_proxy(),
            try
                Path = els_uri:path(Uri),
                {M, EDoc} = edoc:get_doc(
                    els_utils:to_list(Path),
                    [
                        {private, true},
                        edoc_options()
                    ]
                ),
                Internal = xmerl:export_simple([EDoc], docsh_edoc_xmerl),
                %% TODO: Something is weird with the docsh specs.
                %%       For now, let's avoid the Dialyzer warnings.
                Docs = erlang:apply(docsh_docs_v1, from_internal, [Internal]),
                Res = erlang:apply(docsh_docs_v1, lookup, [
                    Docs,
                    {M, F, A},
                    [doc, spec]
                ]),
                flush_group_leader_proxy(GL),

                case Res of
                    {ok, [{{function, F, A}, _Anno, _Signature, Desc, _Metadata} | _]} ->
                        format_edoc(Desc);
                    {not_found, _} ->
                        []
                end
            catch
                C:E:ST ->
                    IO = flush_group_leader_proxy(GL),
                    ?LOG_DEBUG(
                        "[hover] Error fetching edoc [error=~p]",
                        [{M, F, A, C, E, ST, IO}]
                    ),
                    case IO of
                        timeout ->
                            [];
                        noproc ->
                            [];
                        IO ->
                            [{text, IO}]
                    end
            end;
        _ ->
            []
    end.

-spec format_edoc(none | map()) -> [els_markup_content:doc_entry()].
format_edoc(none) ->
    [];
format_edoc(Desc) when is_map(Desc) ->
    Lang = <<"en">>,
    Doc = maps:get(Lang, Desc, <<>>),
    FormattedDoc = els_utils:to_list(docsh_edoc:format_edoc(Doc, #{})),
    [{text, FormattedDoc}].

-spec macro_signature(els_poi:poi_id(), els_arg:args()) -> unicode:charlist().
macro_signature({Name, _Arity}, Args) ->
    [atom_to_list(Name), "(", lists:join(", ", [els_arg:name(A) || A <- Args]), ")"];
macro_signature(Name, none) ->
    atom_to_list(Name).

-spec setup_group_leader_proxy() -> pid().
setup_group_leader_proxy() ->
    OrigGL = group_leader(),
    group_leader(
        spawn_link(
            fun() ->
                spawn_group_proxy([])
            end
        ),
        self()
    ),
    OrigGL.

-spec flush_group_leader_proxy(pid()) -> [term()] | term().
flush_group_leader_proxy(OrigGL) ->
    GL = group_leader(),
    case GL of
        OrigGL ->
            % This is the effect of setting a monitor on nonexisting process.
            noproc;
        _ ->
            Ref = monitor(process, GL),
            group_leader(OrigGL, self()),
            GL ! {get, Ref, self()},
            receive
                {Ref, Msg} ->
                    demonitor(Ref, [flush]),
                    Msg;
                {'DOWN', process, Ref, Reason} ->
                    Reason
            after 5000 ->
                demonitor(Ref, [flush]),
                timeout
            end
    end.

-spec spawn_group_proxy([any()]) -> ok.
spawn_group_proxy(Acc) ->
    receive
        {get, Ref, Pid} ->
            Pid ! {Ref, lists:reverse(Acc)},
            ok;
        {io_request, From, ReplyAs, {put_chars, unicode, Chars}} ->
            From ! {io_reply, ReplyAs, ok},
            spawn_group_proxy([catch unicode:characters_to_binary(Chars) | Acc]);
        {io_request, From, ReplyAs, {put_chars, unicode, M, F, As}} ->
            From ! {io_reply, ReplyAs, ok},
            spawn_group_proxy(
                [catch unicode:characters_to_binary(apply(M, F, As)) | Acc]
            );
        {io_request, From, ReplyAs, _Request} = M ->
            From ! {io_reply, ReplyAs, ok},
            spawn_group_proxy([M | Acc]);
        M ->
            spawn_group_proxy([M | Acc])
    end.

-spec edoc_parse_enabled() -> boolean().
edoc_parse_enabled() ->
    true == els_config:get(edoc_parse_enabled).
