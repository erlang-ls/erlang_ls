-module(els_range).

-include("els_lsp.hrl").

-export([
    compare/2,
    in/2,
    range/4,
    range/1,
    line/1,
    to_poi_range/1,
    inclusion_range/2
]).

-spec compare(poi_range(), poi_range()) -> boolean().
compare(
    #{from := FromA, to := ToA},
    #{from := FromB, to := ToB}
    %% Nested
) when
    FromB =< FromA, ToA =< ToB;
    %% Sequential
    ToA =< FromB;
    %% Sequential & Overlapped
    FromA =< FromB, ToA =< ToB
->
    true;
compare(_, _) ->
    false.

-spec in(poi_range(), poi_range()) -> boolean().
in(#{from := FromA, to := ToA}, #{from := FromB, to := ToB}) ->
    FromA >= FromB andalso ToA =< ToB.

-spec range(pos() | {pos(), pos()} | erl_anno:anno(), poi_kind(), any(), any()) ->
    poi_range().
range({{_Line, _Column} = From, {_ToLine, _ToColumn} = To}, Name, _, _Data) when
    Name =:= export;
    Name =:= export_type;
    Name =:= spec
->
    %% range from unparsable tokens
    #{from => From, to => To};
range({Line, Column}, function_clause, {F, _A, _Index}, _Data) ->
    From = {Line, Column},
    To = plus(From, atom_to_string(F)),
    #{from => From, to => To};
range(Anno, _Type, _Id, _Data) ->
    range(Anno).

-spec range(erl_anno:anno()) -> poi_range().
range(Anno) ->
    From = erl_anno:location(Anno),
    %% To = erl_anno:end_location(Anno),
    To = proplists:get_value(end_location, erl_anno:to_term(Anno)),
    #{from => From, to => To}.

-spec line(poi_range()) -> poi_range().
line(#{from := {FromL, _}, to := {ToL, _}}) ->
    #{from => {FromL, 1}, to => {ToL + 1, 1}}.

%% @doc Converts a LSP range into a POI range
-spec to_poi_range(range()) -> poi_range().
to_poi_range(#{'start' := Start, 'end' := End}) ->
    #{'line' := LineStart, 'character' := CharStart} = Start,
    #{'line' := LineEnd, 'character' := CharEnd} = End,
    #{
        from => {LineStart + 1, CharStart + 1},
        to => {LineEnd + 1, CharEnd + 1}
    };
to_poi_range(#{<<"start">> := Start, <<"end">> := End}) ->
    #{<<"line">> := LineStart, <<"character">> := CharStart} = Start,
    #{<<"line">> := LineEnd, <<"character">> := CharEnd} = End,
    #{
        from => {LineStart + 1, CharStart + 1},
        to => {LineEnd + 1, CharEnd + 1}
    }.

-spec inclusion_range(uri(), els_dt_document:item()) ->
    {ok, poi_range()} | error.
inclusion_range(Uri, Document) ->
    Path = binary_to_list(els_uri:path(Uri)),
    case
        els_compiler_diagnostics:inclusion_range(Path, Document, include) ++
            els_compiler_diagnostics:inclusion_range(Path, Document, include_lib)
    of
        [Range | _] ->
            {ok, Range};
        [] ->
            error
    end.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
    {Line, Column + string:length(String)}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
    io_lib:write(Atom).
