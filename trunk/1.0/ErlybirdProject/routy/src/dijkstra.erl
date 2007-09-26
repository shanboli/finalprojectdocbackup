-module(dijkstra).

-export([table/2,route/2]).

%% Test examples:
%%
%% dijkstra:table([paris, madrid],[{madrid,[berlin]}, {paris, [rome,madrid]}]).
%% dijkstra:table([paris, madrid],[{madrid,[london, berlin]}, {berlin, [stockholm]}, {paris, [rome,madrid]}]).
%%-> [{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]

%% table(Gws, Map) will generate a routing table given a set of
%% gateways and a map. The generated routing table is a list of
%% entries {Dest, Gw} where Gw is the gatways leading to the shortest
%% path.


table(Gws, Map) ->
    Nodes = map:all_nodes(Map),
    Rest = lists:filter(fun (X) -> not lists:member(X, Gws) end, Nodes),
    Direct = lists:map(fun (Nd) -> {Nd,0,Nd} end, Gws),
    Indirect = lists:map(fun (Nd) -> {Nd,inf,na} end, Rest),
    Sorted = lists:append(Direct, Indirect),
    iterate(Sorted, Map, []).
    
%% Test route
%% dijkstra:route(berlin,[{berlin,madrid},{rome,paris},{madrid,madrid},{paris,paris}]).
%% ->{ok,madrid}

route(Node, Table) ->
    case lists:keysearch(Node, 1, Table) of
	{value, {_, Gateway}} ->
	    {ok,Gateway};
	false  ->
	    notfound
    end.
    
 


%% run through the sorted list of shortest paths fond so far and
%% update the rest of entries given the map.

iterate([], _, Table) ->
    Table;
iterate([{_Node, inf, _}|_], _Map, Table) ->
    Table;
iterate([{Node, N, Gw}|Nodes], Map, Table) ->
    Reachable = map:reachable(Node, Map),
    Updated = lists:foldl(fun(Nd, Acc) ->
             update(Nd, Gw, N+1, Acc) end, Nodes, Reachable),
    iterate(Updated, Map, [{Node, Gw}|Table]).


%% If a shorter path can be found then update the entry.

update(Node, Gw, N, Nodes) ->
    M =  entry(Node, Nodes),
    if
	N < M ->
	    replace(Node, N, Gw, Nodes);
	true ->
	    Nodes
    end.


entry(Node, Nodes) ->
    case lists:keysearch(Node, 1, Nodes) of
	{value, {Node, M, _}} ->
	    M;
	false  ->
	    0
end.

replace(Node, N, Gw, Nodes) ->
    insert({Node, N, Gw}, lists:keydelete(Node, 1, Nodes)).

%% Inserting a updated entry at the right position in the list.

insert({Node, X, Gx}, [])->
    [{Node, X, Gx}];
insert({New, X, Gx}, [{Node, Y, Gy}|Rest])  when X < Y ->
    [{New, X, Gx}, {Node, Y, Gy}|Rest];
insert(New, [Node|Rest]) ->
    [Node|insert(New, Rest)].


    
