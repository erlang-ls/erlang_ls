-module(els_fungraph).

-type id_fun(NodeT) :: fun((NodeT) -> _NodeID).
-type edges_fun(NodeT) :: fun((NodeT) -> list(NodeT)).

-opaque graph(NodeT) :: {?MODULE, id_fun(NodeT), edges_fun(NodeT)}.

-export_type([graph/1]).

-export([new/2]).
-export([traverse/4]).

-spec new(id_fun(NodeT), edges_fun(NodeT)) -> graph(NodeT).
new(IdFun, EdgesFun) ->
  {?MODULE, IdFun, EdgesFun}.

-type acc_fun(NodeT, AccT) ::
  fun((_Node :: NodeT, _From :: NodeT, AccT) -> AccT).

-spec traverse(acc_fun(NodeT, AccT), AccT, NodeT, graph(NodeT)) -> AccT.
traverse(AccFun, Acc, From, G) ->
  traverse(AccFun, Acc, [From], sets:new(), G).

-spec traverse(acc_fun(NodeT, AccT), AccT, [NodeT], sets:set(), graph(NodeT)) ->
  AccT.
traverse(AccFun, Acc, [Node | Rest], Visited, G = {?MODULE, IdFun, EdgesFun}) ->
  {AdjacentNodes, VisitedNext} = lists:foldr(
    fun(Adjacent, {NodesAcc, VisitedAcc}) ->
      ID = IdFun(Adjacent),
      case sets:is_element(ID, VisitedAcc) of
        false -> {[Adjacent | NodesAcc], sets:add_element(ID, VisitedAcc)};
        true -> {NodesAcc, VisitedAcc}
      end
    end,
    {[], Visited},
    EdgesFun(Node)
  ),
  AccNext = lists:foldl(
    fun(Adjacent, AccIn) -> AccFun(Adjacent, Node, AccIn) end,
    Acc,
    AdjacentNodes
  ),
  traverse(AccFun, AccNext, AdjacentNodes ++ Rest, VisitedNext, G);
traverse(_AccFun, Acc, [], _Visited, _G) ->
  Acc.
