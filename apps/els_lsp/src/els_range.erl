-module(els_range).

-include("els_lsp.hrl").

-export([ compare/2
        , in/2
        , range/4
        , to_poi_range/1
        ]).

-spec compare(poi_range(), poi_range()) -> boolean().
compare( #{from := FromA, to := ToA}
       , #{from := FromB, to := ToB}
       ) when FromB =< FromA, ToA =< ToB; %% Nested
              ToA =< FromB;               %% Sequential
              FromA =< FromB, ToA =< ToB  %% Sequential & Overlapped
              ->
  true;
compare(_, _) ->
  false.

-spec in(poi_range(), poi_range()) -> boolean().
in(#{from := FromA, to := ToA}, #{from := FromB, to := ToB}) ->
  FromA >= FromB andalso ToA =< ToB.

-spec range(pos() | {pos(), pos()} | erl_anno:anno(), poi_kind(), any(), any())
   -> poi_range().
range({{_Line, _Column} = From, {_ToLine, _ToColumn} = To}, Name, _, _Data)
  when Name =:= export;
       Name =:= export_type;
       Name =:= spec ->
  %% range from unparsable tokens
  #{ from => From, to => To };
range({Line, Column}, function_clause, {F, _A, _Index}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To };
range(Anno, _Type, _Id, _Data) ->
  From = erl_anno:location(Anno),
  %% To = erl_anno:end_location(Anno),
  To = proplists:get_value(end_location, erl_anno:to_term(Anno)),
  #{ from => From, to => To }.

%% @doc Converts a LSP range into a POI range
-spec to_poi_range(range()) -> poi_range().
to_poi_range(#{'start' := Start, 'end' := End}) ->
  #{'line' := LineStart, 'character' := CharStart} = Start,
   #{'line' := LineEnd, 'character' := CharEnd} = End,
  #{ from => {LineStart + 1, CharStart + 1}
   , to => {LineEnd + 1, CharEnd + 1}
   };
to_poi_range(#{<<"start">> := Start, <<"end">> := End}) ->
  #{<<"line">> := LineStart, <<"character">> := CharStart} = Start,
  #{<<"line">> := LineEnd, <<"character">> := CharEnd} = End,
  #{ from => {LineStart + 1, CharStart + 1}
   , to => {LineEnd + 1, CharEnd + 1}
   }.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + string:length(String)}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
  io_lib:write(Atom).
