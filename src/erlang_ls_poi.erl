%%==============================================================================
%% The Point Of Interest (a.k.a. _poi_) Data Structure
%%==============================================================================
-module(erlang_ls_poi).

-type line()   :: non_neg_integer().
-type column() :: non_neg_integer().
-type pos()    :: {line(), column()}.
-type range()  :: #{ from := pos(), to := pos() }.
-type poi()    :: #{ type := atom()
                   , info => any()
                   , range := range()
                   }.

-export_type([ poi/0
             , range/0
             ]).

-export([ poi/2 ]).

-export([ list/1
        , match/2
        , match_key/2
        , match_pos/2
        ]).

%%==============================================================================
%% API
%%==============================================================================

%% @edoc Constructor for a Point of Interest.
-spec poi(erlang_ls_tree:tree(), any()) -> poi().
poi(Tree, Info) ->
  Pos = erl_syntax:get_pos(Tree),
  Range = get_range(Pos, Info),
  #{ type  => poi
   , info  => Info
   , range => Range
   }.

%% @edoc List the Points of Interest for a given tree.
-spec list(erlang_ls_tree:tree()) -> [poi()].
list(Tree) ->
  F = fun(T, Acc) ->
          Annotations = erl_syntax:get_ann(T),
          case [POI || #{ type := poi } = POI <- Annotations] of
            [] -> Acc;
            L -> L ++ Acc
          end
      end,
  erl_syntax_lib:fold(F, [], Tree).

-spec match(erlang_ls_tree:tree(), any()) -> [poi()].
match(Tree, Info0) ->
  [POI || #{info := Info} = POI <- list(Tree), Info0 =:= Info].

-spec match_key(erlang_ls_tree:tree(), atom()) -> [poi()].
match_key(Tree, Key0) ->
  [POI || #{info := {Key, _}} = POI <- list(Tree), Key0 =:= Key].

-spec match_pos(erlang_ls_tree:tree(), pos()) -> [poi()].
match_pos(Tree, Pos) ->
  [POI || #{range := Range} = POI <- list(Tree), matches_pos(Pos, Range)].

%%==============================================================================
%% Internal Functions
%%==============================================================================

-spec get_range(pos(), {atom(), any()}) -> range().
get_range({Line, Column}, {application, {M, F, _A}}) ->
  CFrom = Column - length(atom_to_list(M)),
  From = {Line, CFrom},
  CTo = Column + length(atom_to_list(F)),
  To = {Line, CTo},
  #{ from => From, to => To };
get_range({Line, Column}, {application, {F, _A}}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(F))},
  #{ from => From, to => To };
get_range({Line, Column}, {behaviour, Behaviour}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("behaviour") + length(atom_to_list(Behaviour))},
  #{ from => From, to => To };
get_range({_Line, _Column}, {exports_entry, {_F, _A}}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} };
get_range({Line, Column}, {function, {F, _A}}) ->
  From = {Line - 1, Column - 1},
  To = {Line - 1, Column + length(atom_to_list(F)) - 1},
  #{ from => From, to => To };
get_range({Line, _Column}, {define, _Define}) ->
  From = {Line - 1, 0},
  To = From,
  #{ from => From, to => To };
get_range({Line, Column}, {include, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, {include_lib, Include}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length("include_lib") + length(Include)},
  #{ from => From, to => To };
get_range({Line, Column}, {macro, Macro}) ->
  From = {Line, Column},
  To = {Line, Column + length(atom_to_list(Macro))},
  #{ from => From, to => To };
get_range({Line, Column}, {module, _}) ->
  From = {Line - 1, Column - 1},
  To = From,
  #{ from => From, to => To };
get_range({Line, Column}, {record_expr, Record}) ->
  From = {Line, Column - 1},
  To = {Line, Column + length(Record) - 1},
  #{ from => From, to => To };
%% TODO: Distinguish between usage poi and definition poi
get_range({Line, _Column}, {record, _Record}) ->
  From = {Line - 1, 0},
  To = From,
  #{ from => From, to => To };
get_range({_Line, _Column}, {spec, _Spec}) ->
  %% TODO: The location information for the arity qualifiers are lost during
  %%       parsing in `epp_dodger`. This requires fixing.
  #{ from => {0, 0}, to => {0, 0} }.

-spec matches_pos(pos(), range()) -> boolean().
matches_pos(Pos, #{from := From, to := To}) ->
  (From =< Pos) andalso (Pos =< To).
