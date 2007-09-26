-module(hist).

-export([new/1,update/3]).



new(Name) ->
    [{Name,inf}].



update(Node, N, History) ->
    case lists:keysearch(Node, 1, History) of
	{value, {CityName, Count}} ->
	    if N < Count ->
                old;
               N >= Count ->
                lists:keyreplace(CityName,1,History,{CityName,N})
            end;            
	false ->
	    notfound
    end.

