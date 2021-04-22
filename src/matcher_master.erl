-module(matcher_master).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([set_config/1,match/1]).
-include("common.hrl").



init(_Args) ->
  WorkerNum = application:get_env(smartlb,matcher_worker,1),
  WorkerPid = [matcher_worker:start_link() || _ <- lists:seq(1,WorkerNum)],
  {ok,#{
	worker_list => WorkerPid,
	worker_index => 1,
	worker_num => WorkerNum
       }}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).


handle_call({set_config,Funcs},_From,#{worker_list:=WL}=State) ->
  broadcast({update_config,Funcs},WL),
  {reply,ok,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({match_request,_,_}=Msg,#{worker_list:=WL,worker_index:=I}=State) ->
  N = length(WL),
  II = (I rem N) + 1,
  WorkerPid = lists:nth(I,WL),
  WorkerPid ! Msg,
  {noreply,State#{worker_index:=II}};
handle_cast(_Request,State) ->
  {noreply,State}.


broadcast(Msg,Workerlist) ->
  lists:foreach(
    fun(Pid) ->
	Pid ! Msg
    end,Workerlist).

%% ################# API ##################

%% set_config assume the argument is valid, no validation is done here
-spec set_config([function()]) -> ok | error.
set_config(FlowFuncs) ->
  gen_server:call(?MODULE,{set_config,FlowFuncs}).

%% for performance reason, use async instead of gen_sever:call
%% match_result response would be sent to requesting process later
-spec match(binary()) -> ok.
match(Data) ->
  gen_server:cast(?MODULE,{match_request,Data,self()}).

%% ################# API ##################
