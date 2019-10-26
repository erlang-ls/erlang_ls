-module(erlang_ls_range).

-include("erlang_ls.hrl").

-export([ range/4 ]).

-spec range(pos(), poi_kind(), any(), extra()) -> poi_range().
range({Line, Column}, application, {M, F, _A}, _Extra) ->
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  CTo = Column + length(atom_to_list(F)),
  To = {Line, CTo},
  #{ from => From, to => To };
range({Line, Column}, application, {F, _A}, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {M, F, A}, _Extra) ->
  From = {Line, Column},
  %% Assumes "fun M:F/A"
  Length = 6 + length(atom_to_list(M) ++ atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {F, A}, _Extra) ->
  From = {Line, Column},
  %% Assumes "fun F/A"
  Length = 5 + length(atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
range({Line, Column}, behaviour, Behaviour, _Extra) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
range({_Line, _Column}, exports_entry, {F, A}, Extra) ->
  get_entry_range(exports_locations, F, A, Extra);
range({_Line, _Column}, import_entry, {_M, F, A}, Extra) ->
  get_entry_range(import_locations, F, A, Extra);
range({Line, Column}, function, {F, _A}, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
range({Line, _Column}, define, _Define, _Extra) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, include, Include, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
range({Line, Column}, include_lib, Include, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
range({Line, Column}, macro, Macro, _Extra) when is_atom(Macro) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
range({Line, Column}, module, _, _Extra) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
range(Pos, record_access, {Record, Field}, _Extra) ->
  #{ from => minus(Pos, "#"), to => plus(Pos, Record ++ "." ++ Field) };
range({Line, Column}, record_expr, Record, _Extra) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
%% TODO: Distinguish between usage poi and definition poi
range({Line, _Column}, record, _Record, _Extra) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
range({Line, Column}, spec, _, _Extra) ->
  #{ from => {Line, Column}
   , to => {Line, Column}
   };
%% TODO: Do we really need the StartLocation there?
range({_Line, _Column}, type_application, {Type, StartLocation}, _Extra) ->
  {FromLine, FromColumn} = From = StartLocation,
  Length = length(atom_to_list(Type)),
  To = {FromLine, FromColumn + Length - 1},
  #{ from => From, to => To };
range({Line, Column}, type_definition, _Type, _Extra) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To }.

-spec get_entry_range(atom(), atom(), non_neg_integer(), extra()) ->
   poi_range().
get_entry_range(Key, F, A, Extra) ->
  Locations = maps:get(Key, Extra, []),
  {FromLine, FromColumn} = proplists:get_value({F, A}, Locations),
  From = {FromLine, FromColumn - 1},
  Length = length(atom_to_list(F)) + length(integer_to_list(A)) + 1,
  To = {FromLine, FromColumn + Length - 1},
  #{ from => From, to => To }.

-spec minus(pos(), string()) -> pos().
minus({Line, Column}, String) ->
  {Line, Column - length(String) - 1}.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + length(String)}.
