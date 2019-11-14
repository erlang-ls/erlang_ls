-module(els_range).

-include("erlang_ls.hrl").

-export([ range/3 ]).

-spec range(pos(), poi_kind(), any()) -> poi_range().
range({Line, Column}, application, {M, F, _A}) ->
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  CTo = Column + length(atom_to_list(F)),
  To = {Line, CTo},
  #{ from => From, to => To };
range({Line, Column}, application, {F, _A}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {M, F, A}) ->
  From = {Line, Column},
  %% Assumes "fun M:F/A"
  Length = 6 + length(atom_to_list(M) ++ atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {F, A}) ->
  From = {Line, Column},
  %% Assumes "fun F/A"
  Length = 5 + length(atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, behaviour, Behaviour) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
range(Pos, export_entry, {F, A}) ->
  get_entry_range(Pos, F, A);
range(Pos, import_entry, {_M, F, A}) ->
  get_entry_range(Pos, F, A);
range({Line, Column}, function, {F, _A}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, _Column}, define, _Define) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, include, Include) ->
  From = {Line, Column},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
range({Line, Column}, include_lib, Include) ->
  From = {Line, Column},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
range({Line, Column}, macro, Macro) when is_atom(Macro) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
range({Line, Column}, module, _) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
range(Pos, record_access, {Record, Field}) ->
  #{ from => minus(Pos, "#"), to => plus(Pos, Record ++ "." ++ Field) };
range({Line, Column}, record_expr, Record) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
range({Line, _Column}, record, _Record) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, spec, _) ->
  #{ from => {Line, Column}
   , to => {Line, Column}
   };
range({Line, Column}, type_application, {F, _A}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(atom_to_list(F)) - 1},
  #{ from => From, to => To };
range({Line, Column}, type_application, {M, F, _A}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(atom_to_list(M)), + length(atom_to_list(F)) - 1},
  #{ from => From, to => To };
range({Line, Column}, type_definition, _Type) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, variable, Name) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Name))},
  #{ from => From, to => To }.

-spec get_entry_range(pos(), atom(), non_neg_integer()) -> poi_range().
get_entry_range({Line, Column}, F, A) ->
  From = {Line, Column - 1},
  Length = length(atom_to_list(F)) + length(integer_to_list(A)) + 1,
  To = {Line, Column + Length - 1},
  #{ from => From, to => To }.

-spec minus(pos(), string()) -> pos().
minus({Line, Column}, String) ->
  {Line, Column - length(String) - 1}.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + length(String)}.
