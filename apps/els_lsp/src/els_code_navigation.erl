%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(els_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([
    goto_definition/2,
    find_in_scope/2
]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("els_lsp.hrl").
-include_lib("kernel/include/logger.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec goto_definition(uri(), els_poi:poi()) ->
    {ok, [{uri(), els_poi:poi()}]} | {error, any()}.
goto_definition(
    Uri,
    Var = #{kind := variable}
) ->
    %% This will naively try to find the definition of a variable by finding the
    %% first occurrence of the variable in variable scope.
    case find_in_scope(Uri, Var) of
        [Var | _] -> {error, already_at_definition};
        [POI | _] -> {ok, [{Uri, POI}]};
        % Probably due to parse error
        [] -> {error, nothing_in_scope}
    end;
goto_definition(
    _Uri,
    #{kind := Kind, id := {M, F, A}}
) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= import_entry
->
    case els_utils:find_module(M) of
        {ok, Uri} -> defs_to_res(find(Uri, function, {F, A}));
        {error, Error} -> {error, Error}
    end;
goto_definition(
    Uri,
    #{kind := Kind, id := {F, A}} = POI
) when
    Kind =:= application;
    Kind =:= implicit_fun;
    Kind =:= export_entry
->
    %% try to find local function first
    %% fall back to bif search if unsuccessful
    case find(Uri, function, {F, A}) of
        [] ->
            case is_imported_bif(Uri, F, A) of
                true ->
                    goto_definition(Uri, POI#{id := {erlang, F, A}});
                false ->
                    {error, not_found}
            end;
        Result ->
            defs_to_res(Result)
    end;
goto_definition(
    Uri,
    #{kind := atom, id := Id}
) ->
    %% Two interesting cases for atoms: functions and modules.
    %% We return all function defs with any arity combined with module defs.
    DefsFun = find(Uri, function, {Id, any_arity}),
    case els_utils:find_module(Id) of
        {ok, ModUri} -> defs_to_res(DefsFun ++ find(ModUri, module, Id));
        {error, _Error} -> defs_to_res(DefsFun)
    end;
goto_definition(
    _Uri,
    #{kind := Kind, id := Module}
) when
    Kind =:= behaviour;
    Kind =:= module
->
    case els_utils:find_module(Module) of
        {ok, Uri} -> defs_to_res(find(Uri, module, Module));
        {error, Error} -> {error, Error}
    end;
goto_definition(
    Uri,
    #{
        kind := macro,
        id := {MacroName, _Arity} = Define
    } = POI
) ->
    case find(Uri, define, Define) of
        [] ->
            goto_definition(Uri, POI#{id => MacroName});
        Else ->
            defs_to_res(Else)
    end;
goto_definition(Uri, #{kind := macro, id := Define}) ->
    defs_to_res(find(Uri, define, Define));
goto_definition(Uri, #{kind := record_expr, id := Record}) ->
    defs_to_res(find(Uri, record, Record));
goto_definition(Uri, #{kind := record_field, id := {Record, Field}}) ->
    defs_to_res(find(Uri, record_def_field, {Record, Field}));
goto_definition(_Uri, #{kind := Kind, id := Id}) when
    Kind =:= include;
    Kind =:= include_lib
->
    case els_utils:find_header(els_utils:filename_to_atom(Id)) of
        {ok, Uri} -> {ok, [{Uri, beginning()}]};
        {error, Error} -> {error, Error}
    end;
goto_definition(_Uri, #{kind := type_application, id := {M, T, A}}) ->
    case els_utils:find_module(M) of
        {ok, Uri} -> defs_to_res(find(Uri, type_definition, {T, A}));
        {error, Error} -> {error, Error}
    end;
goto_definition(Uri, #{kind := Kind, id := {T, A}}) when
    Kind =:= type_application; Kind =:= export_type_entry
->
    defs_to_res(find(Uri, type_definition, {T, A}));
goto_definition(_Uri, #{kind := parse_transform, id := Module}) ->
    case els_utils:find_module(Module) of
        {ok, Uri} -> defs_to_res(find(Uri, module, Module));
        {error, Error} -> {error, Error}
    end;
goto_definition(_Filename, _) ->
    {error, not_found}.

-spec is_imported_bif(uri(), atom(), non_neg_integer()) -> boolean().
is_imported_bif(_Uri, F, A) ->
    OldBif = erl_internal:old_bif(F, A),
    Bif = erl_internal:bif(F, A),
    case {OldBif, Bif} of
        %% Cannot be shadowed, always imported
        {true, true} ->
            true;
        %% It's not a BIF at all
        {false, false} ->
            false;
        %% The hard case, just jump to the bif for now
        {_, _} ->
            true
    end.

-spec defs_to_res([{uri(), els_poi:poi()}]) -> {ok, [{uri(), els_poi:poi()}]} | {error, not_found}.
defs_to_res([]) -> {error, not_found};
defs_to_res(Defs) -> {ok, Defs}.

-spec find(uri() | [uri()], els_poi:poi_kind(), any()) ->
    [{uri(), els_poi:poi()}].
find(UriOrUris, Kind, Data) ->
    find(UriOrUris, Kind, Data, sets:new()).

-spec find(uri() | [uri()], els_poi:poi_kind(), any(), sets:set(binary())) ->
    [{uri(), els_poi:poi()}].
find([], _Kind, _Data, _AlreadyVisited) ->
    [];
find([Uri | Uris0], Kind, Data, AlreadyVisited) ->
    case sets:is_element(Uri, AlreadyVisited) of
        true ->
            find(Uris0, Kind, Data, AlreadyVisited);
        false ->
            AlreadyVisited2 = sets:add_element(Uri, AlreadyVisited),
            case els_utils:lookup_document(Uri) of
                {ok, Document} ->
                    find_in_document([Uri | Uris0], Document, Kind, Data, AlreadyVisited2);
                {error, _Error} ->
                    find(Uris0, Kind, Data, AlreadyVisited2)
            end
    end;
find(Uri, Kind, Data, AlreadyVisited) ->
    find([Uri], Kind, Data, AlreadyVisited).

-spec find_in_document(
    uri() | [uri()],
    els_dt_document:item(),
    els_poi:poi_kind(),
    any(),
    sets:set(binary())
) ->
    [{uri(), els_poi:poi()}].
find_in_document([Uri | Uris0], Document, Kind, Data, AlreadyVisited) ->
    POIs = els_dt_document:pois(Document, [Kind]),
    Defs = [POI || #{id := Id} = POI <- POIs, Id =:= Data],
    {AllDefs, MultipleDefs} =
        case Data of
            {_, any_arity} when Kind =:= function ->
                %% Including defs with any arity
                AnyArity = [
                    POI
                 || #{id := {F, _}} = POI <- POIs, Kind =:= function, Data =:= {F, any_arity}
                ],
                {AnyArity, true};
            _ ->
                {Defs, false}
        end,
    case AllDefs of
        [] ->
            case maybe_imported(Document, Kind, Data) of
                [] ->
                    find(
                        lists:usort(include_uris(Document) ++ Uris0),
                        Kind,
                        Data,
                        AlreadyVisited
                    );
                Else ->
                    Else
            end;
        Definitions ->
            SortedDefs = els_poi:sort(Definitions),
            if
                MultipleDefs ->
                    %% This will be the case only when the user tries to navigate to the definition of an atom
                    [{Uri, POI} || POI <- SortedDefs];
                true ->
                    %% In the general case, we return only one def
                    [{Uri, hd(SortedDefs)}]
            end
    end.

-spec include_uris(els_dt_document:item()) -> [uri()].
include_uris(Document) ->
    POIs = els_dt_document:pois(Document, [include, include_lib]),
    lists:foldl(fun add_include_uri/2, [], POIs).

-spec add_include_uri(els_poi:poi(), [uri()]) -> [uri()].
add_include_uri(#{id := Id}, Acc) ->
    case els_utils:find_header(els_utils:filename_to_atom(Id)) of
        {ok, Uri} -> [Uri | Acc];
        {error, _Error} -> Acc
    end.

-spec beginning() -> #{range => #{from => {1, 1}, to => {1, 1}}}.
beginning() ->
    #{range => #{from => {1, 1}, to => {1, 1}}}.

%% @doc check for a match in any of the module imported functions.
-spec maybe_imported(els_dt_document:item(), els_poi:poi_kind(), any()) ->
    [{uri(), els_poi:poi()}].
maybe_imported(Document, function, {F, A}) ->
    POIs = els_dt_document:pois(Document, [import_entry]),
    case [{M, F, A} || #{id := {M, FP, AP}} <- POIs, FP =:= F, AP =:= A] of
        [] ->
            [];
        [{M, F, A} | _] ->
            case els_utils:find_module(M) of
                {ok, Uri0} -> find(Uri0, function, {F, A});
                {error, not_found} -> []
            end
    end;
maybe_imported(_Document, _Kind, _Data) ->
    [].

-spec find_in_scope(uri(), els_poi:poi()) -> [els_poi:poi()].
find_in_scope(Uri, #{kind := variable, id := VarId, range := VarRange}) ->
    {ok, Document} = els_utils:lookup_document(Uri),
    VarPOIs = els_poi:sort(els_dt_document:pois(Document, [variable])),
    ScopeRange = els_scope:variable_scope_range(VarRange, Document),
    [
        POI
     || #{range := Range, id := Id} = POI <- VarPOIs,
        els_range:in(Range, ScopeRange),
        Id =:= VarId
    ].
