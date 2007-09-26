-module(intf).

-export([new/0,add/4,remove/2,lookup/2,ref/2,name/2,list/1,broadcast/2]).

%% Test examples:
%%
%% dijkstra:table([paris, madrid],[{madrid,[berlin]}, {paris, [rome,madrid]}]).
%% dijkstra:table([paris, madrid],[{madrid,[london, berlin]}, {berlin, [stockholm]}, {paris, [rome,madrid]}]).

%% table(Gws, Map) will generate a routing table given a set of
%% gateways and a map. The generated routing table is a list of
%% entries {Dest, Gw} where Gw is the gatways leading to the shortest
%% path.

new() ->
    [].

add(Name,Ref,Pid,Intf) ->
    [{Name, Ref, Pid} | Intf].
    
remove(Name,Intf) ->
    Filter = fun(Elem) -> {ElemName, _Ref, _Pid} = Elem, ElemName =/= Name end,
    lists:filter(Filter, Intf).
    
lookup(Name,Intf) ->
    case lists:keysearch(Name, 1, Intf) of
        {value, {_Name, _Ref, Pid}} ->
            {ok, Pid};
	false ->
            notfound
    end.
    
ref(Name, Intf) ->
    case lists:keysearch(Name, 1, Intf) of
        {value, {_Name, Ref, _Pid}} ->
            {ok, Ref};
	false ->
            notfound
    end.
    
name(Ref, Intf) ->
    case lists:keysearch(Ref, 2, Intf) of
        {value, {Name, _Ref, _Pid}} ->
            {ok, Name};
	false ->
            notfound
    end.

list(Intf) ->
    Fun = fun({L1,L2,L3}) -> {Iwant,_,_} = {L1,L2,L3},Iwant end,
    lists:map(Fun,Intf).
    
broadcast(Message,Intf) ->
    Fun = fun({L1,L2,L3}) -> {_,_,Pid} = {L1,L2,L3}, Pid ! Message end,
    lists:map(Fun,Intf).
    
    



    
