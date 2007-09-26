%%%-------------------------------------------------------------------
%%% Created : 2007��9��25��
%%%-------------------------------------------------------------------
-module(routy).

-behaviour(application).

-export([
	 start/2,
	 stop/1
        ]).



%%====================================================================
%% External functions
%%====================================================================
%%--------------------------------------------------------------------
%% Func: start/2
%% Returns: {ok, Pid}        |
%%          {ok, Pid, State} |
%%          {error, Reason}
%%--------------------------------------------------------------------
start(Type, StartArgs) ->
    case 'TopSupervisor':start_link(StartArgs) of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
    end.

%%--------------------------------------------------------------------
%% Func: stop/1
%% Returns: any
%%--------------------------------------------------------------------
stop(State) ->
    ok.

