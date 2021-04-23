-module(matcher_master).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,start_link/0]).
-export([set_config/1,match/1]).

-ifdef(TEST).
-export([kill_worker/0]).
-endif.

-include("common.hrl").

%% the matcher master is nothing but entrypoint of match request, configuration updating,
%% it load balance the requests to workers, supervise workers
%%
%% the API contract: clients call match/1 of matcher_master, and will get async notify
%% {match_result,Result :: {match,M} | need_more | nomatch}
%% this cause inconvenience but can dispatch match request among workers that is
%% useful to extend CPU power under heavy load

init(_Args) ->
  WorkerNum = application:get_env(smartlb,matcher_worker,1),
  WorkerPid = [matcher_worker:start_link() || _ <- lists:seq(1,WorkerNum)],
  process_flag(trap_exit,true),
  WorkerArray = array:new([{size,length(WorkerPid)},{fixed,true}]),
  {WA,_} = lists:foldl(
    fun(Pid,{Array,Index}) ->
	NewArray = array:set(Index,Pid,Array),
	{NewArray,Index + 1}
    end,{WorkerArray,0},WorkerPid),
  {ok,#{
	worker_array => WA,
	worker_index => 0,
	worker_num => WorkerNum,
	flow_funcs => []
       }}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).


handle_call({set_config,Funcs},_From,#{worker_array:=WL}=State) ->
  broadcast({update_config,Funcs},WL),
  {reply,ok,State#{flow_funcs:=Funcs}};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({match_request,_,_}=Msg,State) ->
  {WorkerPid,NewState} = select_worker(State),
  WorkerPid ! Msg,
  {noreply,NewState};
handle_cast(test_kill,State) ->
  {WorkerPid,NewState} = select_worker(State),
  WorkerPid ! test_kill,
  {noreply,NewState};
handle_cast(_Request,State) ->
  {noreply,State}.

select_worker(#{worker_num:=N,worker_array:=WL,worker_index:=I}=State) ->
  II = (I+1) rem N,
  {array:get(I,WL),State#{worker_index:=II}}.



handle_info({'EXIT',Pid,Reason},#{worker_array:=WA,flow_funcs:=FF}=State) ->
  logger:error("matcher worker ~p died with reason ~p",[Pid,Reason]),
  %% restart the died worker process
  WL = array:to_list(WA),
  NewWA =
    case utils:index_of(Pid,WL) of
      none ->
	logger:error("master worker with pid ~p died but cannot be found",[Pid]),
	WA;
      Position ->
	%% found died worker's pid position, restart a new process and replace it
	NPid = matcher_worker:start_link(),
	%% and notify the latest config
	NPid ! {broadcast,Position,{update_config,FF}},
	array:set(Position,NPid,WA)
    end,
  {noreply,State#{worker_array:=NewWA}};
handle_info(_,State) ->
  {noreply,State}.

broadcast(Msg,WorkerArray) ->
  %% array is 0-based
  array:sparse_foldl(
    fun(Index,Pid,Acc) ->
	Pid ! {broadcast,Index,Msg},
	Acc
    end,0,WorkerArray).

%% ################# API ##################

%% set_config assume the argument is valid, no validation is done here
-spec set_config([function()]) -> ok | error.
set_config(FlowFuncs) ->
  gen_server:call(?MODULE,{set_config,FlowFuncs}).

%% for performance reason, use async instead of gen_sever:call
%% match_result response would be sent to requesting process later
%% matched result is {host,port}
-spec match(binary()) -> {match,{string(),integer()}} | need_more | nomatch.
match(Data) ->
  gen_server:cast(?MODULE,{match_request,Data,self()}),
  %% selectively waiting for match result
  receive
    {match_result,Result} ->
      Result
  after
    3000 ->
      logger:error("match master wait for result timeout"),
      nomatch
  end.

-ifdef(TEST).
kill_worker() ->
  gen_server:cast(?MODULE,test_kill).
-endif.

%% ################# API ##################
