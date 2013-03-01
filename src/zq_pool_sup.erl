-module(zq_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Handler,Num,Context,Endpoints) ->
    {ok,Pid} = supervisor:start_link(?MODULE, []),

    lists:foreach(fun(_)->
      lager:info("starting zq worker"),
      {ok,_} = supervisor:start_child(Pid,[Handler,Context,Endpoints])
    end,lists:seq(1,Num)),

    {ok,Pid}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {simple_one_for_one, 5, 10}, [
      {undefined, {zq_pool_worker, start_link, []}, permanent, 5000, worker, [zq_pool_worker]}
    ]} }.

