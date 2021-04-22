-module(matcher_worker).
-export([loop/1,start_link/0]).


start_link() ->
  spawn_link(?MODULE,loop,[ [] ]).

loop(FlowFuncs) ->
  F = receive
	{match_request,Data,Pid} ->
	  Result = execute_flow(Data,FlowFuncs),
	  Pid ! {match_result,Result},
	  FlowFuncs;
	{update_config,NewFlowFuncs} ->
	  NewFlowFuncs;
	Other ->
	  logger:error("matcher_work receive unknown msg: ~p",[Other]),
	  FlowFuncs
      end,
  loop(F).


-spec execute_flow(binary(),[function()]) -> {match,tuple()} | nomatch.
execute_flow(Data,FlowFuncs) ->
  execute_flow(Data,FlowFuncs,#{}).
execute_flow(_,[],_) -> nomatch;
execute_flow(Data,[H|T],Cache) ->
  case H(Data,Cache) of
    {nomatch,NewCache} ->
      execute_flow(Data,T,NewCache);
    {match,Host} -> {match,Host}
  end.
