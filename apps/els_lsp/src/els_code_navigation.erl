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
        {ok, Uri} -> find(Uri, function, {F, A});
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
        {error, Error} ->
            case is_imported_bif(Uri, F, A) of
                true ->
                    goto_definition(Uri, POI#{id := {erlang, F, A}});
                false ->
                    {error, Error}
            end;
        Result ->
            Result
    end;
goto_definition(
    Uri,
    #{kind := atom, id := Id} = POI
) ->
    %% Two interesting cases for atoms: functions and modules.
    %% If we can't find functions with any arity, we hope that the atom refers to a module.
    %% All definitions are returned to the user
    case find(Uri, function, {Id, -1}) of
        {error, _Error} -> goto_definition(Uri, POI#{kind := module});
        {ok, DefsFun} ->
            case find(Uri, module, Id) of
                {error, _Error} -> {ok, DefsFun};
                {ok, DefsMod} ->
                    {ok, DefsFun ++ DefsMod}
            end
    end;
goto_definition(
    _Uri,
    #{kind := Kind, id := Module}
) when
    Kind =:= behaviour;
    Kind =:= module
->
    case els_utils:find_module(Module) of
        {ok, Uri} -> find(Uri, module, Module);
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
        {error, not_found} ->
            goto_definition(Uri, POI#{id => MacroName});
        Else ->
            Else
    end;
goto_definition(Uri, #{kind := macro, id := Define}) ->
    find(Uri, define, Define);
goto_definition(Uri, #{kind := record_expr, id := Record}) ->
    find(Uri, record, Record);
goto_definition(Uri, #{kind := record_field, id := {Record, Field}}) ->
    find(Uri, record_def_field, {Record, Field});
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
        {ok, Uri} -> find(Uri, type_definition, {T, A});
        {error, Error} -> {error, Error}
    end;
goto_definition(Uri, #{kind := Kind, id := {T, A}}) when
    Kind =:= type_application; Kind =:= export_type_entry
->
    find(Uri, type_definition, {T, A});
goto_definition(_Uri, #{kind := parse_transform, id := Module}) ->
    case els_utils:find_module(Module) of
        {ok, Uri} -> find(Uri, module, Module);
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

-spec find(uri() | [uri()], els_poi:poi_kind(), any()) ->
    {ok, [{uri(), els_poi:poi()}]} | {error, not_found}.
find(UriOrUris, Kind, Data) ->
    find(UriOrUris, Kind, Data, sets:new()).

-spec find(uri() | [uri()], els_poi:poi_kind(), any(), sets:set(binary())) ->
    {ok, [{uri(), els_poi:poi()}]} | {error, not_found}.
find([], _Kind, _Data, _AlreadyVisited) ->
    {error, not_found};
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
    {ok, [{uri(), els_poi:poi()}]} | {error, any()}.
find_in_document([Uri | Uris0], Document, Kind, Data, AlreadyVisited) ->
    POIs = els_dt_document:pois(Document, [Kind]),
    Defs = [POI || #{id := Id} = POI <- POIs, Id =:= Data],
    {AllDefs, MultipleDefs} =
    case Data of
     {_, -1} when Kind =:= function ->
        %% Including defs with any arity
        AnyArity = [POI || #{id := {F, _}} = POI <- POIs, Kind =:= function, Data =:= {F, -1}],
        {AnyArity, true};
     _ ->
        {Defs, false}
    end,
    case AllDefs of
        [] ->
            case maybe_imported(Document, Kind, Data) of
                {ok, IDefs} ->
                    {ok, IDefs};
                {error, not_found} ->
                    find(
                        lists:usort(include_uris(Document) ++ Uris0),
                        Kind,
                        Data,
                        AlreadyVisited
                    )
            end;
        Definitions ->
            SortedDefs = els_poi:sort(Definitions),
            if
                MultipleDefs ->
                    %% This will be the case only when the user tries to navigate to the definition of an atom
                    {ok, [{Uri, POI} || POI <- SortedDefs]};
                true ->
                    %% In the general case, we return only one def
                    {ok, [{Uri, hd(SortedDefs)}]}
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
    {ok, [{uri(), els_poi:poi()}]} | {error, not_found}.
maybe_imported(Document, function, {F, A}) ->
    POIs = els_dt_document:pois(Document, [import_entry]),
    case [{M, F, A} || #{id := {M, FP, AP}} <- POIs, FP =:= F, AP =:= A] of
        [] ->
            {error, not_found};
        [{M, F, A} | _] ->
            case els_utils:find_module(M) of
                {ok, Uri0} -> find(Uri0, function, {F, A});
                {error, not_found} -> {error, not_found}
            end
    end;
maybe_imported(_Document, _Kind, _Data) ->
    {error, not_found}.

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
