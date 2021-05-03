-module(els_range).

-include("els_lsp.hrl").

-export([ compare/2
        , in/2
        , range/4
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

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + string:length(String)}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
  io_lib:write(Atom).
