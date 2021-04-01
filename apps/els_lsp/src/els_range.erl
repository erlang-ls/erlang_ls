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

-spec range(pos() | {pos(), pos()}, poi_kind(), any(), any()) -> poi_range().
range({{Line, Column}, {ToLine, ToColumn}}, Name, _, _Data)
  when Name =:= folding_range;
       Name =:= spec ->
  %% -1 as we include the "-" before spec.
  From = {Line, Column - 1},
  %% +1 as we include the . after the spec
  To = {ToLine, ToColumn - 1 + 1},
  #{ from => From, to => To };
range({{_Line, _Column} = From, {_ToLine, _ToColumn} = To}, Name, _, _Data)
  when Name =:= export;
       Name =:= export_type ->
  #{ from => From, to => To };
range(Pos, export_entry, {F, A}, _Data) ->
  get_entry_range(Pos, F, A);
range(Pos, import_entry, {_M, F, A}, _Data) ->
  get_entry_range(Pos, F, A);
range({Line, Column}, export_type_entry, {F, A}, _Data) ->
  get_entry_range({Line, Column}, F, A);
range({_Line, _Column} = From, atom, Name, _Data) ->
  To = plus(From, atom_to_string(Name)),
  #{ from => From, to => To };
range({Line, Column}, application, {_, F, A}, #{imported := true} = Data) ->
  range({Line, Column}, application, {F, A}, Data);
range({Line, Column}, application, {M, F, _A}, _Data) ->
  %% Column indicates the position of the :
  CFrom = Column - string:length(atom_to_string(M)),
  From = {Line, CFrom},
  %% module:function
  CTo = Column + string:length(atom_to_string(F)) + 1,
  To = {Line, CTo},
  #{ from => From, to => To };
range({Line, Column}, application, {F, _A}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {M, F, A}, _Data) ->
  From = {Line, Column},
  %% Assumes "fun M:F/A"
  To = plus(From, "fun " ++ atom_to_string(M) ++ ":" ++
                atom_to_string(F) ++ "/" ++ integer_to_list(A)),
  #{ from => From, to => To };
range({Line, Column}, implicit_fun, {F, A}, _Data) ->
  From = {Line, Column},
  %% Assumes "fun F/A"
  To = plus(From, "fun " ++ atom_to_string(F) ++ "/" ++ integer_to_list(A)),
  #{ from => From, to => To };
range({Line, Column}, behaviour, Behaviour, _Data) ->
  From = {Line, Column - 1},
  To = plus(From, "-behaviour(" ++ atom_to_string(Behaviour) ++ ")."),
  #{ from => From, to => To };
range({Line, Column}, callback, {F, _A}, _Data) ->
  From = {Line, Column - 1},
  To = plus(From, "-callback " ++ atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, function, {F, _A}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, function_clause, {F, _A, _Index}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, define, Define, _Data) ->
  From = plus({Line, Column}, "define("),
  To = plus(From, atom_to_list(Define)),
  #{ from => From, to => To };
range({Line, Column}, include, Include, _Data) ->
  From = {Line, Column - 1},
  To = plus(From, "-include(\"" ++ Include ++ "\")."),
  #{ from => From, to => To };
range({Line, Column}, include_lib, Include, _Data) ->
  From = {Line, Column - 1},
  To = plus(From, "-include_lib(\"" ++ Include ++ "\")."),
  #{ from => From, to => To };
range({Line, Column}, macro, Macro, _Data) when is_atom(Macro) ->
  From = {Line, Column},
  To = plus(From, "?" ++ atom_to_list(Macro)),
  #{ from => From, to => To };
range({Line, Column}, module, Module, _Data) ->
  %% The Column we get is of the 'm' in the -module pragma
  From = plus({Line, Column}, "module("),
  To = plus(From, atom_to_string(Module)),
  #{ from => From, to => To };
range({Line, Column}, parse_transform, PT, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(PT)),
  #{ from => From, to => To };
range({Line, Column}, record_expr, Record, _Data) ->
  %% the range includes the leading '#'
  From = {Line, Column},
  #{ from => From, to => plus(From, "#" ++ atom_to_string(Record)) };
range(Pos, record_field, {_Record, Field}, _Data) ->
  From = Pos,
  #{ from => From, to => plus(From, atom_to_string(Field)) };
range(Pos, record_def_field, {_Record, Field}, _Data) ->
  From = Pos,
  #{ from => From, to => plus(From, atom_to_string(Field)) };
range({Line, Column}, record, Record, _Data) ->
  From = plus({Line, Column}, "record("),
  To = plus(From, atom_to_string(Record)),
  #{ from => From, to => To };
range({Line, Column}, type_application, {F, _A}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, type_application, {M, F, _A}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(M) ++ ":" ++ atom_to_string(F)),
  #{ from => From, to => To };
range({Line, Column}, type_definition, {Name, _}, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_string(Name)),
  #{ from => From, to => To };
range({Line, Column}, variable, Name, _Data) ->
  From = {Line, Column},
  To = plus(From, atom_to_list(Name)),
  #{ from => From, to => To }.

-spec get_entry_range(pos(), atom(), non_neg_integer()) -> poi_range().
get_entry_range({Line, Column}, F, A) ->
  From = {Line, Column},
  %% length("function/arity")
  Length = string:length(atom_to_string(F) ++ "/" ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To }.

%% -spec minus(pos(), string()) -> pos().
%% minus({Line, Column}, String) ->
%%   {Line, Column - string:length(String)}.

-spec plus(pos(), string()) -> pos().
plus({Line, Column}, String) ->
  {Line, Column + string:length(String)}.

-spec atom_to_string(atom()) -> string().
atom_to_string(Atom) ->
  io_lib:write(Atom).
