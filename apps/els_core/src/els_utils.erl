-module(els_utils).

-export([
    cmd/2,
    cmd/3,
    filename_to_atom/1,
    find_header/1,
    find_module/1,
    find_modules/1,
    fold_files/4,
    halt/1,
    lookup_document/1,
    include_id/1,
    include_lib_id/1,
    macro_string_to_term/1,
    project_relative/1,
    resolve_paths/2,
    to_binary/1,
    to_list/1,
    compose_node_name/2,
    function_signature/1,
    base64_encode_term/1,
    base64_decode_term/1,
    levenshtein_distance/2,
    camel_case/1,
    jaro_distance/2,
    is_windows/0,
    system_tmp_dir/0,
    race/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_core.hrl").
-include_lib("kernel/include/logger.hrl").

-type path() :: file:filename_all().

%%==============================================================================
%% File and module functions
%%==============================================================================

-spec cmd(string(), [string()]) -> integer() | no_return().
cmd(Cmd, Args) ->
    cmd(Cmd, Args, []).

% @doc Replacement for os:cmd that allows for spaces in args and paths
-spec cmd(string(), [string()], string()) -> integer() | no_return().
cmd(Cmd, Args, Path) ->
    ?LOG_INFO("Running OS command [command=~p] [args=~p]", [Cmd, Args]),
    Executable =
        case filename:basename(Cmd) of
            Cmd ->
                cmd_path(Cmd);
            _ ->
                %% The command already contains a path
                Cmd
        end,
    Tag = make_ref(),
    F =
        fun() ->
            P = open_port(
                {spawn_executable, Executable},
                [
                    binary,
                    use_stdio,
                    stream,
                    exit_status,
                    hide,
                    {args, Args},
                    %% TODO: Windows-friendly version?
                    {env, [{"PATH", Path ++ ":" ++ os:getenv("PATH")}]}
                ]
            ),
            exit({Tag, cmd_receive(P)})
        end,
    {Pid, Ref} = erlang:spawn_monitor(F),
    receive
        {'DOWN', Ref, process, Pid, {Tag, Data}} ->
            Data;
        {'DOWN', Ref, process, Pid, Reason} ->
            exit(Reason)
    end.

%% @doc Return the path for a command
-spec cmd_path(string()) -> string().
cmd_path(Cmd) ->
    ErtsBinDir = filename:dirname(escript:script_name()),
    case os:find_executable(Cmd, ErtsBinDir) of
        false ->
            case os:find_executable(Cmd) of
                false ->
                    Fmt = "Could not find command ~p",
                    Args = [Cmd],
                    Msg = lists:flatten(io_lib:format(Fmt, Args)),
                    error(Msg);
                GlobalEpmd ->
                    GlobalEpmd
            end;
        Epmd ->
            Epmd
    end.

%% @doc Convert an 'include'/'include_lib' POI ID to a document index ID
-spec filename_to_atom(string()) -> atom().
filename_to_atom(FileName) ->
    list_to_atom(filename:basename(FileName, filename:extension(FileName))).

%% @doc Look for a header in the DB
-spec find_header(atom()) -> {ok, uri()} | {error, any()}.
find_header(Id) ->
    {ok, Candidates} = els_dt_document_index:lookup(Id),
    case [Uri || #{kind := header, uri := Uri} <- Candidates] of
        [Uri | _] ->
            {ok, Uri};
        [] ->
            FileName = atom_to_list(Id) ++ ".hrl",
            els_indexing:find_and_deeply_index_file(FileName)
    end.

%% @doc Look for a module in the DB
-spec find_module(atom()) -> {ok, uri()} | {error, not_found}.
find_module(Id) ->
    case find_modules(Id) of
        {ok, [Uri | _]} ->
            {ok, Uri};
        {ok, []} ->
            {error, not_found}
    end.

%% @doc Look for all versions of a module in the DB
-spec find_modules(atom()) -> {ok, [uri()]}.
find_modules(Id) ->
    {ok, Candidates} = els_dt_document_index:lookup(Id),
    case [Uri || #{kind := module, uri := Uri} <- Candidates] of
        [] ->
            FileName = atom_to_list(Id) ++ ".erl",
            case els_indexing:find_and_deeply_index_file(FileName) of
                {ok, Uri} ->
                    {ok, [Uri]};
                _Error ->
                    ?LOG_INFO("Finding module failed [filename=~p]", [FileName]),
                    {ok, []}
            end;
        Uris ->
            {ok, prioritize_uris(Uris)}
    end.

%% @doc Look for a document in the DB.
%%
%% Look for a given document in the DB and return it.
%% If the module is not in the DB, try to index it.
-spec lookup_document(uri()) ->
    {ok, els_dt_document:item()} | {error, any()}.
lookup_document(Uri0) ->
    case els_dt_document:lookup(Uri0) of
        {ok, [Document]} ->
            {ok, Document};
        {ok, []} ->
            Path = els_uri:path(Uri0),
            %% The returned Uri could be different from the original input
            %% (e.g. if the original Uri would contain a query string)
            {ok, Uri} = els_indexing:shallow_index(Path, app),
            case els_dt_document:lookup(Uri) of
                {ok, [Document]} ->
                    {ok, Document};
                {ok, []} ->
                    ?LOG_INFO("Document lookup failed [uri=~p]", [Uri]),
                    {error, document_lookup_failed}
            end
    end.

%% @doc Convert path to an 'include' POI id
-spec include_id(file:filename_all()) -> string().
include_id(Path) ->
    els_utils:to_list(filename:basename(Path)).

%% @doc Convert path to an 'include_lib' POI id
-spec include_lib_id(file:filename_all()) -> string().
include_lib_id(Path) ->
    Components = filename:split(Path),
    Length = length(Components),
    End = Length - 1,
    Beginning = max(1, Length - 2),
    [H | T] = lists:sublist(Components, Beginning, End),
    %% Strip the app version number from the path
    Id = filename:join([re:replace(H, "-.*", "", [{return, list}]) | T]),
    els_utils:to_list(Id).

-spec macro_string_to_term(list()) -> any().
macro_string_to_term(Value) ->
    try
        {ok, Tokens, _End} = erl_scan:string(Value ++ "."),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _Class:Exception ->
            Fmt =
                "Error parsing custom defined macro, "
                "falling back to 'true'"
                "[value=~p] [exception=~p]",
            Args = [Value, Exception],
            ?LOG_ERROR(Fmt, Args),
            true
    end.

%% @doc Folds over all files in a directory
%%
%% Applies function F to each file and the accumulator,
%% skipping all symlinks.
-spec fold_files(function(), function(), path(), any()) -> any().
fold_files(F, Filter, Dir, Acc) ->
    do_fold_dir(F, Filter, Dir, Acc).

%% @doc Resolve paths based on path specs
%%
%% Gets a list of path specs and returns the expanded list of paths.
%% Path specs can contains glob expressions.
-spec resolve_paths([[path()]], boolean()) -> [[path()]].
resolve_paths(PathSpecs, Recursive) ->
    lists:append([
        resolve_path(PathSpec, Recursive)
     || PathSpec <- PathSpecs
    ]).

-spec halt(non_neg_integer()) -> ok.
halt(ExitCode) ->
    ok = init:stop(ExitCode).

%% @doc Returns a project-relative file path for a given URI
-spec project_relative(uri()) -> file:filename() | {error, not_relative}.
project_relative(Uri) ->
    RootUri = els_config:get(root_uri),
    Size = byte_size(RootUri),
    case Uri of
        <<RootUri:Size/binary, Relative/binary>> ->
            Trimmed = string:trim(Relative, leading, [$/, $\\]),
            to_list(Trimmed);
        _ ->
            {error, not_relative}
    end.

-spec to_binary(unicode:chardata()) -> binary().
to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_list(X) ->
    case unicode:characters_to_binary(X) of
        Result when is_binary(Result) -> Result;
        _ -> iolist_to_binary(X)
    end.

-spec to_list(unicode:chardata()) -> string().
to_list(X) when is_list(X) ->
    X;
to_list(X) when is_binary(X) ->
    case unicode:characters_to_list(X) of
        Result when is_list(Result) -> Result;
        _ -> binary_to_list(X)
    end.

-spec is_windows() -> boolean().
is_windows() ->
    {OS, _} = os:type(),
    OS =:= win32.

-spec system_tmp_dir() -> string().
system_tmp_dir() ->
    case is_windows() of
        true ->
            os:getenv("TEMP");
        false ->
            "/tmp"
    end.

%% @doc Run functions in parallel and return the result of the first function
%%      that terminates
-spec race([fun(() -> Result)], timeout()) -> Result.
race(Funs, Timeout) ->
    Parent = self(),
    Ref = make_ref(),
    Pids = [spawn_link(fun() -> Parent ! {Ref, Fun()} end) || Fun <- Funs],
    receive
        {Ref, Result} ->
            %% Ensure no lingering processes
            [exit(Pid, kill) || Pid <- Pids],
            %% Ensure no lingering messages
            ok = flush(Ref),
            Result
    after Timeout ->
        %% Ensure no lingering processes
        [exit(Pid, kill) || Pid <- Pids],
        %% Ensure no lingering messages
        ok = flush(Ref),
        error(timeout)
    end.

%%==============================================================================
%% Internal functions
%%==============================================================================
-spec flush(reference()) -> ok.
flush(Ref) ->
    receive
        {Ref, _} ->
            flush(Ref)
    after 0 ->
        ok
    end.

%% Folding over files

-spec do_fold_files(function(), function(), path(), [path()], any()) -> any().
do_fold_files(_F, _Filter, _Dir, [], Acc0) ->
    Acc0;
do_fold_files(F, Filter, Dir, [File | Rest], Acc0) ->
    Path = filename:join(Dir, File),
    %% Symbolic links are not regular files
    Acc =
        case filelib:is_regular(Path) of
            true -> do_fold_file(F, Filter, Path, Acc0);
            false -> do_fold_dir(F, Filter, Path, Acc0)
        end,
    do_fold_files(F, Filter, Dir, Rest, Acc).

-spec do_fold_file(function(), function(), path(), any()) ->
    any().
do_fold_file(F, Filter, Path, Acc) ->
    case Filter(Path) of
        true -> F(Path, Acc);
        false -> Acc
    end.

-spec do_fold_dir(function(), function(), path(), any()) ->
    any().
do_fold_dir(F, Filter, Dir, Acc) ->
    case not is_symlink(Dir) andalso filelib:is_dir(Dir) of
        true ->
            {ok, Files} = file:list_dir(Dir),
            do_fold_files(F, Filter, Dir, Files, Acc);
        false ->
            Acc
    end.

-spec is_symlink(path()) -> boolean().
is_symlink(Path) ->
    case file:read_link(Path) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%% @doc Resolve paths recursively

-spec resolve_path([path()], boolean()) -> [path()].
resolve_path(PathSpec, Recursive) ->
    Path = filename:join(PathSpec),
    Paths = filelib:wildcard(Path),

    case Recursive of
        true ->
            lists:append([
                [make_normalized_path(P) | subdirs(P)]
             || P <- Paths
            ]);
        false ->
            [make_normalized_path(P) || P <- Paths]
    end.

%% Returns all subdirectories for the provided path
-spec subdirs(path()) -> [path()].
subdirs(Path) ->
    subdirs(Path, []).

-spec subdirs(path(), [path()]) -> [path()].
subdirs(Path, Subdirs) ->
    case file:list_dir(Path) of
        {ok, Files} -> subdirs_(Path, Files, Subdirs);
        {error, _} -> Subdirs
    end.

-spec subdirs_(path(), [path()], [path()]) -> [path()].
subdirs_(Path, Files, Subdirs) ->
    Fold = fun(F, Acc) ->
        FullPath = filename:join([Path, F]),
        case
            not is_symlink(FullPath) andalso
                filelib:is_dir(FullPath)
        of
            true -> subdirs(FullPath, [FullPath | Acc]);
            false -> Acc
        end
    end,
    lists:foldl(Fold, Subdirs, Files).

-spec cmd_receive(port()) -> integer().
cmd_receive(Port) ->
    receive
        {Port, {exit_status, ExitCode}} ->
            ExitCode;
        {Port, _} ->
            cmd_receive(Port)
    end.

%% @doc Prioritize files
%% Prefer files below root and prefer files in src dir.
-spec prioritize_uris([uri()]) -> [uri()].
prioritize_uris(Uris) ->
    Root = els_config:get(root_uri),
    AppsPaths = els_config:get(apps_paths),
    Prio = [{score_uri(Uri, Root, AppsPaths), Uri} || Uri <- Uris],
    [Uri || {_, Uri} <- lists:sort(Prio)].

-spec score_uri(uri(), uri(), [file:name()]) -> tuple().
score_uri(Uri, RootUri, AppsPaths) ->
    Path = els_uri:path(Uri),
    Prefix = string:prefix(Uri, RootUri),
    %% prefer files under project root
    S1 =
        case Prefix of
            nomatch -> 1;
            _Rest -> 0
        end,
    %% among those, prefer files under some project app directory (e.g.
    %% deprioritize dependencies and shadow copies)
    S2 = length([
        AP
     || S1 == 0,
        AP <- AppsPaths,
        string:prefix(Path, AP) /= nomatch
    ]),
    {S1, -S2}.

%%==============================================================================
%% This section excerpted from the rebar3 sources, rebar_dir.erl
%% Pending resolution of https://github.com/erlang/rebar3/issues/2223
%%==============================================================================

%% @doc make a path absolute
-spec make_absolute_path(path()) -> path().
make_absolute_path(Path) ->
    case filename:pathtype(Path) of
        absolute ->
            Path;
        relative ->
            {ok, Dir} = file:get_cwd(),
            filename:join([Dir, Path]);
        volumerelative ->
            Volume = hd(filename:split(Path)),
            {ok, Dir} = file:get_cwd(Volume),
            filename:join([Dir, Path])
    end.

%% @doc normalizing a path removes all of the `..' and the
%% `.' segments it may contain.
-spec make_normalized_path(path()) -> path().
make_normalized_path(Path) ->
    AbsPath = make_absolute_path(Path),
    Components = filename:split(AbsPath),
    make_normalized_path(Components, []).

%% @private drops path fragments for normalization
-spec make_normalized_path([file:name_all()], [file:name_all()]) -> path().
make_normalized_path([], NormalizedPath) ->
    filename:join(lists:reverse(NormalizedPath));
make_normalized_path(["." | []], []) ->
    ".";
make_normalized_path(["." | T], NormalizedPath) ->
    make_normalized_path(T, NormalizedPath);
make_normalized_path([".." | T], []) ->
    make_normalized_path(T, [".."]);
make_normalized_path([".." | T], [Head | Tail]) when Head =/= ".." ->
    make_normalized_path(T, Tail);
make_normalized_path([H | T], NormalizedPath) ->
    make_normalized_path(T, [H | NormalizedPath]).

-spec compose_node_name(Name :: string(), Type :: shortnames | longnames) ->
    NodeName :: atom().
compose_node_name(Name, Type) ->
    NodeName =
        case lists:member($@, Name) of
            true ->
                Name;
            _ ->
                HostName = els_config_runtime:get_hostname(),
                Name ++ [$@ | HostName]
        end,
    case Type of
        shortnames ->
            list_to_atom(NodeName);
        longnames ->
            Domain = els_config_runtime:get_domain(),
            case Domain of
                "" -> list_to_atom(NodeName);
                _ -> list_to_atom(NodeName ++ "." ++ Domain)
            end
    end.

%% @doc Given an MFA or a FA, return a printable version of the
%% function signature, in binary format.
-spec function_signature(
    {atom(), atom(), non_neg_integer()}
    | {atom(), non_neg_integer()}
) ->
    binary().
function_signature({M, F, A}) ->
    els_utils:to_binary(io_lib:format("~p:~ts/~p", [M, F, A]));
function_signature({F, A}) ->
    els_utils:to_binary(io_lib:format("~ts/~p", [F, A])).

-spec base64_encode_term(any()) -> binary().
base64_encode_term(Term) ->
    els_utils:to_binary(base64:encode_to_string(term_to_binary(Term))).

-spec base64_decode_term(binary()) -> any().
base64_decode_term(Base64) ->
    binary_to_term(base64:decode(Base64)).

-spec camel_case(binary() | string()) -> binary().
camel_case(Str0) ->
    %% Remove ''
    Str = string:trim(Str0, both, "'"),
    Words = [string:titlecase(Word) || Word <- string:lexemes(Str, "_")],
    iolist_to_binary(Words).

-spec levenshtein_distance(binary(), binary()) -> integer().
levenshtein_distance(S, T) ->
    {Distance, _} = levenshtein_distance(to_list(S), to_list(T), #{}),
    Distance.

-spec levenshtein_distance(string(), string(), map()) -> {integer(), map()}.
levenshtein_distance([] = S, T, Cache) ->
    {length(T), maps:put({S, T}, length(T), Cache)};
levenshtein_distance(S, [] = T, Cache) ->
    {length(S), maps:put({S, T}, length(S), Cache)};
levenshtein_distance([X | S], [X | T], Cache) ->
    levenshtein_distance(S, T, Cache);
levenshtein_distance([_SH | ST] = S, [_TH | TT] = T, Cache) ->
    case maps:find({S, T}, Cache) of
        {ok, Distance} ->
            {Distance, Cache};
        error ->
            {L1, C1} = levenshtein_distance(S, TT, Cache),
            {L2, C2} = levenshtein_distance(ST, T, C1),
            {L3, C3} = levenshtein_distance(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, maps:put({S, T}, L, C3)}
    end.

%%% Jaro distance

%% @doc Computes the Jaro distance (similarity) between two strings.
%%
%%   Returns a float value between 0.0 (equates to no similarity) and 1.0 (is an
%%   exact match) representing Jaro distance between String1 and String2.
%%
%%   The Jaro distance metric is designed and best suited for short strings such
%%   as person names. Erlang LS uses this function to provide the "did you
%%   mean?" functionality.
%%
%% @end
-spec jaro_distance(S, S) -> float() when S :: string() | binary().
jaro_distance(Str, Str) ->
    1.0;
jaro_distance(_, "") ->
    0.0;
jaro_distance("", _) ->
    0.0;
jaro_distance(Str1, Str2) when
    is_binary(Str1),
    is_binary(Str2)
->
    jaro_distance(
        binary_to_list(Str1),
        binary_to_list(Str2)
    );
jaro_distance(Str1, Str2) when
    is_list(Str1),
    is_list(Str2)
->
    Len1 = length(Str1),
    Len2 = length(Str2),
    case jaro_match(Str1, Len1, Str2, Len2) of
        {0, _Trans} ->
            0.0;
        {Comm, Trans} ->
            (Comm / Len1 + Comm / Len2 + (Comm - Trans) / Comm) / 3
    end.

-type jaro_state() :: {integer(), integer(), integer()}.
-type jaro_range() :: {integer(), integer()}.

-spec jaro_match(string(), integer(), string(), integer()) ->
    {integer(), integer()}.
jaro_match(Chars1, Len1, Chars2, Len2) when Len1 < Len2 ->
    jaro_match(Chars1, Chars2, (Len2 div 2) - 1);
jaro_match(Chars1, Len1, Chars2, _Len2) ->
    jaro_match(Chars2, Chars1, (Len1 div 2) - 1).

-spec jaro_match(string(), string(), integer()) -> {integer(), integer()}.
jaro_match(Chars1, Chars2, Lim) ->
    jaro_match(Chars1, Chars2, {0, Lim}, {0, 0, -1}, 0).

-spec jaro_match(string(), string(), jaro_range(), jaro_state(), integer()) ->
    {integer(), integer()}.
jaro_match([Char | Rest], Chars0, Range, State0, Idx) ->
    {Chars, State} = jaro_submatch(Char, Chars0, Range, State0, Idx),
    case Range of
        {Lim, Lim} ->
            jaro_match(Rest, tl(Chars), Range, State, Idx + 1);
        {Pre, Lim} ->
            jaro_match(Rest, Chars, {Pre + 1, Lim}, State, Idx + 1)
    end;
jaro_match([], _, _, {Comm, Trans, _}, _) ->
    {Comm, Trans}.

-spec jaro_submatch(char(), string(), jaro_range(), jaro_state(), integer()) ->
    {string(), jaro_state()}.
jaro_submatch(Char, Chars0, {Pre, _} = Range, State, Idx) ->
    case jaro_detect(Char, Chars0, Range) of
        undefined ->
            {Chars0, State};
        {SubIdx, Chars} ->
            {Chars, jaro_proceed(State, Idx - Pre + SubIdx)}
    end.

-spec jaro_detect(char(), string(), jaro_range()) ->
    {integer(), string()} | undefined.
jaro_detect(Char, Chars, {Pre, Lim}) ->
    jaro_detect(Char, Chars, Pre + 1 + Lim, 0, []).

-spec jaro_detect(char(), string(), integer(), integer(), list()) ->
    {integer(), string()} | undefined.
jaro_detect(_Char, _Chars, 0, _Idx, _Acc) ->
    undefined;
jaro_detect(_Char, [], _Lim, _Idx, _Acc) ->
    undefined;
jaro_detect(Char, [Char | Rest], _Lim, Idx, Acc) ->
    {Idx, lists:reverse(Acc) ++ [undefined | Rest]};
jaro_detect(Char, [Other | Rest], Lim, Idx, Acc) ->
    jaro_detect(Char, Rest, Lim - 1, Idx + 1, [Other | Acc]).

-spec jaro_proceed(jaro_state(), integer()) -> jaro_state().
jaro_proceed({Comm, Trans, Former}, Current) when Current < Former ->
    {Comm + 1, Trans + 1, Current};
jaro_proceed({Comm, Trans, _Former}, Current) ->
    {Comm + 1, Trans, Current}.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

jaro_distance_test_() ->
    [
        ?_assertEqual(
            jaro_distance("same", "same"),
            1.0
        ),
        ?_assertEqual(
            jaro_distance("any", ""),
            0.0
        ),
        ?_assertEqual(
            jaro_distance("", "any"),
            0.0
        ),
        ?_assertEqual(
            jaro_distance("martha", "marhta"),
            0.9444444444444445
        ),
        ?_assertEqual(
            jaro_distance("martha", "marhha"),
            0.888888888888889
        ),
        ?_assertEqual(
            jaro_distance("marhha", "martha"),
            0.888888888888889
        ),
        ?_assertEqual(
            jaro_distance("dwayne", "duane"),
            0.8222222222222223
        ),
        ?_assertEqual(
            jaro_distance("dixon", "dicksonx"),
            0.7666666666666666
        ),
        ?_assertEqual(
            jaro_distance("xdicksonx", "dixon"),
            0.7851851851851852
        ),
        ?_assertEqual(
            jaro_distance("shackleford", "shackelford"),
            0.9696969696969697
        ),
        ?_assertEqual(
            jaro_distance("dunningham", "cunnigham"),
            0.8962962962962964
        ),
        ?_assertEqual(
            jaro_distance("nichleson", "nichulson"),
            0.9259259259259259
        ),
        ?_assertEqual(
            jaro_distance("jones", "johnson"),
            0.7904761904761904
        ),
        ?_assertEqual(
            jaro_distance("massey", "massie"),
            0.888888888888889
        ),
        ?_assertEqual(
            jaro_distance("abroms", "abrams"),
            0.888888888888889
        ),
        ?_assertEqual(
            jaro_distance("hardin", "martinez"),
            0.7222222222222222
        ),
        ?_assertEqual(
            jaro_distance("itman", "smith"),
            0.4666666666666666
        ),
        ?_assertEqual(
            jaro_distance("jeraldine", "geraldine"),
            0.9259259259259259
        ),
        ?_assertEqual(
            jaro_distance("michelle", "michael"),
            0.8690476190476191
        ),
        ?_assertEqual(
            jaro_distance("julies", "julius"),
            0.888888888888889
        ),
        ?_assertEqual(
            jaro_distance("tanya", "tonya"),
            0.8666666666666667
        ),
        ?_assertEqual(
            jaro_distance("sean", "susan"),
            0.7833333333333333
        ),
        ?_assertEqual(
            jaro_distance("jon", "john"),
            0.9166666666666666
        ),
        ?_assertEqual(
            jaro_distance("jon", "jan"),
            0.7777777777777777
        ),
        ?_assertEqual(
            jaro_distance("семена", "стремя"),
            0.6666666666666666
        )
    ].

camel_case_test() ->
    ?assertEqual(<<"">>, camel_case(<<"">>)),
    ?assertEqual(<<"F">>, camel_case(<<"f">>)),
    ?assertEqual(<<"Foo">>, camel_case(<<"foo">>)),
    ?assertEqual(<<"FooBar">>, camel_case(<<"foo_bar">>)),
    ?assertEqual(<<"FooBarBaz">>, camel_case(<<"foo_bar_baz">>)),
    ?assertEqual(<<"FooBarBaz">>, camel_case(<<"'foo_bar_baz'">>)).

-endif.
