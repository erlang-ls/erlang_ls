-module(els_range).

-include("erlang_ls.hrl").

-export([ compare/2
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

-spec range(pos(), poi_kind(), any(), any()) -> poi_range().
range({Line, Column}, application, {M, F, _A}, _Data) ->
  %% Column indicates the position of the :
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  %% module:function
  CTo = Column + length(atom_to_list(F)) + 1,
  To = {Line, CTo},
  #{ from => From, to => To };
range({Line, Column}, application, {F, _A}, _Data) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {M, F, A}, _Data) ->
  From = {Line, Column},
  %% Assumes "fun M:F/A"
  Length = 6 + length(atom_to_list(M) ++ atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {F, A}, _Data) ->
  From = {Line, Column},
  %% Assumes "fun F/A"
  Length = 5 + length(atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, behaviour, Behaviour, _Data) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
range({Line, Column}, exports, {ToLine, ToColumn}, _Data) ->
  From = {Line, Column - 1},
  To = {ToLine, ToColumn - 1},
  #{ from => From, to => To };
range(Pos, export_entry, {F, A}, _Data) ->
  get_entry_range(Pos, F, A);
range(Pos, import_entry, {_M, F, A}, _Data) ->
  get_entry_range(Pos, F, A);
range({Line, Column}, function, {F, _A}, _Data) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, Column}, define, Define, _Data) ->
  From = plus({Line, Column}, "define("),
  To = plus(From, atom_to_list(Define)),
  #{ from => From, to => To };
range({Line, Column}, include, Include, _Data) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include") + length(Include) + 5},
  #{ from => From, to => To };
range({Line, Column}, include_lib, Include, _Data) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include_lib") + length(Include) + 5},
  #{ from => From, to => To };
range({Line, Column}, macro, Macro, _Data) when is_atom(Macro) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
range({Line, Column}, module, _, _Data) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
range({Line, _Column}, parse_transform, _Define, _Data) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
range(Pos, record_access, Record, Field) ->
  #{ from => minus(Pos, "#")
   , to => plus(Pos, atom_to_list(Record) ++ "." ++ atom_to_list(Field)) };
range({Line, Column}, record_expr, Record, _Data) ->
  From = {Line, Column - 1},
  To = plus({Line, Column}, atom_to_list(Record)),
  #{ from => From, to => To };
range({Line, Column}, record, Record, _Data) ->
  From = plus({Line, Column}, "record("),
  To = plus(From, atom_to_list(Record)),
  #{ from => From, to => To };
range({Line, Column}, spec, _, _Data) ->
  #{ from => {Line, Column}
   , to => {Line, Column}
   };
range({Line, Column}, type_application, {F, _A}, _Data) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(atom_to_list(F)) - 1},
  #{ from => From, to => To };
range({Line, Column}, type_application, {M, F, _A}, _Data) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(atom_to_list(M)) + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, Column}, type_definition, _Type, _Data) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, variable, Name, _Data) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Name))},
  #{ from => From, to => To }.

-spec get_entry_range(pos(), atom(), non_neg_integer()) -> poi_range().
get_entry_range({Line, Column}, F, A) ->
  From = {Line, Column - 1},
  %% length("function/arity")
  Length = length(atom_to_list(F)) + length(integer_to_list(A)) + 1,
  To = {Line, Column + Length - 1},
  #{ from => From, to => To }.

-spec minus(pos(), string()) -> pos().
minus({Line, Column}, String) ->
  {Line, Column - length(String) - 1}.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + length(String)}.
