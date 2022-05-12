-module(els_fungraph_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0]).

-export([dense_graph_traversal/1]).

-spec all() -> [atom()].
all() ->
    [dense_graph_traversal].

-spec dense_graph_traversal(_Config) -> ok.
dense_graph_traversal(_) ->
    MaxID = 40,
    % Visited nodes must be unique
    ?assertEqual(
        lists:reverse(lists:seq(1, MaxID)),
        els_fungraph:traverse(
            fun(ID, _From, Acc) -> [ID | Acc] end,
            [],
            1,
            els_fungraph:new(
                fun(ID) -> ID end,
                fun(ID) ->
                    % Each node has edges to nodes in range [ID; ID * 2]
                    [NextID || NextID <- lists:seq(ID, ID * 2), NextID =< MaxID]
                end
            )
        )
    ).
