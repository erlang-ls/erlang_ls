%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(erlang_ls_poi).

-type line()   :: non_neg_integer().
-type column() :: non_neg_integer().
-type pos()    :: {line(), column()}.
-type range()  :: #{ from := pos(), to := pos() }.
-type kind()   :: application
                | behaviour
                | define
                | exports_entry
                | function
                | implicit_fun
                | import_entry
                | include
                | include_lib
                | macro
                | module
                | record
                | record_access
                | record_expr
                | type_application
                | type_definition.
-type poi()    :: #{ kind  => kind()
                   , data  => any()
                   , range := range()
                   }.

-export_type([ kind/0
             , poi/0
             , range/0
             ]).

-export([ poi/4 ]).

-export([ list/1
        , match_pos/2
        , first/1
        ]).

%%==============================================================================
%% API
%%==============================================================================

%% @edoc Constructor for a Point of Interest.
-spec poi(erlang_ls_tree:tree(), kind(), any(), erlang_ls_tree:extra()) -> poi().
poi(Tree, Kind, Data, Extra) ->
  Pos = erl_syntax:get_pos(Tree),
  Range = get_range(Pos, Kind, Data, Extra),
  #{ kind  => Kind
   , data  => Data
   , range => Range
   }.

%% TODO: Really needed?
%% @edoc List the Points of Interest for a given tree.
-spec list(erlang_ls_tree:tree()) -> [poi()].
list(Tree) ->
  F = fun(T, Acc) ->
          case erl_syntax:get_ann(T) of
            [] -> Acc;
            L -> L ++ Acc
          end
      end,
  erl_syntax_lib:fold(F, [], Tree).

-spec match_pos(erlang_ls_tree:tree(), pos()) -> [poi()].
match_pos(Tree, Pos) ->
  [POI || #{range := Range} = POI <- list(Tree), matches_pos(Pos, Range)].

-spec first([poi()]) -> poi().
first([]) ->
  error(badarg);
first(POIs) ->
  [First | _] = lists:sort(fun compare_pos/2, POIs),
  First.

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec get_range(pos(), kind(), any(), erlang_ls_tree:extra()) -> range().
get_range({Line, Column}, application, {M, F, _A}, _Extra) ->
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  CTo = Column + length(atom_to_list(F)),
  To = {Line, CTo},
  #{ from => From, to => To };
get_range({Line, Column}, application, {F, _A}, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
get_range({Line, Column}, implicit_fun, {M, F, A}, _Extra) ->
  From = {Line, Column},
  %% Assumes "fun M:F/A"
  Length = 6 + length(atom_to_list(M) ++ atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
get_range({Line, Column}, implicit_fun, {F, A}, _Extra) ->
  From = {Line, Column},
  %% Assumes "fun F/A"
  Length = 5 + length(atom_to_list(F) ++ integer_to_list(A)),
  To = {Line, Column + Length},
  #{ from => From, to => To };
get_range({Line, Column}, behaviour, Behaviour, _Extra) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
get_range({_Line, _Column}, exports_entry, {F, A}, Extra) ->
  get_entry_range(exports_locations, F, A, Extra);
get_range({_Line, _Column}, import_entry, {_M, F, A}, Extra) ->
  get_entry_range(import_locations, F, A, Extra);
get_range({Line, Column}, function, {F, _A}, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
get_range({Line, _Column}, define, _Define, _Extra) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
get_range({Line, Column}, include, Include, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, include_lib, Include, _Extra) ->
  From = {Line, Column},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, macro, Macro, _Extra) when is_atom(Macro) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
get_range({Line, Column}, module, _, _Extra) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To };
get_range(Pos, record_access, {Record, Field}, _Extra) ->
  #{ from => minus(Pos, "#"), to => plus(Pos, Record ++ "." ++ Field) };
get_range({Line, Column}, record_expr, Record, _Extra) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
%% TODO: Distinguish between usage poi and definition poi
get_range({Line, _Column}, record, _Record, _Extra) ->
  From = {Line, 1},
  To = From,
  #{ from => From, to => To };
%% TODO: Do we really need the StartLocation there?
get_range({_Line, _Column}, type_application, {Type, StartLocation}, _Extra) ->
  {FromLine, FromColumn} = From = StartLocation,
  Length = length(atom_to_list(Type)),
  To = {FromLine, FromColumn + Length - 1},
  #{ from => From, to => To };
get_range({Line, Column}, type_definition, _Type, _Extra) ->
  From = {Line, Column},
  To = From,
  #{ from => From, to => To }.

-spec matches_pos(pos(), range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).

-spec compare_pos(poi(), poi()) -> boolean().
compare_pos(#{range := #{from := From1}}, #{range := #{from := From2}}) ->
  (From1 =< From2).

-spec get_entry_range(atom(), atom(), non_neg_integer(), erlang_ls_tree:extra()) -> range().
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
