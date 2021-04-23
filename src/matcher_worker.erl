-module(matcher_worker).
-export([loop/2,start_link/0]).

-type match_result() :: {match,tuple()} | need_more | nomatch.
-type flow_state() :: map().  % flow state persist between each update_config

start_link() ->
  spawn_link(?MODULE,loop,[ [], #{} ]).

loop(FlowFuncs,S) ->
  {F,NewState} =
    receive
      {match_request,Data,Pid} ->
	{Result,State1} = execute_flow(Data,FlowFuncs,S),
	Pid ! {match_result,Result},
	{FlowFuncs,State1};
      {broadcast,WorkerId,Msg} ->
	case Msg of
	  {update_config,NewFlowFuncs} ->
	    {NewFlowFuncs, #{worker_id=>WorkerId}}	% use new state
	end;
      test_kill ->
	exit(test_kill);
      Other ->
	logger:error("matcher_work receive unknown msg: ~p",[Other]),
	{FlowFuncs,S}
    end,
  loop(F,NewState).


-spec execute_flow(binary(),[function()],flow_state()) -> {match_result(),flow_state()}.
execute_flow(Data,FlowFuncs,State) ->
  execute_flow(Data,FlowFuncs,#{},State).
execute_flow(_,[],_,State) -> {nomatch,State};
execute_flow(Data,[H|T],Cache,State) ->
  case H(Data,Cache,State) of
    {nomatch,NewCache,S1} ->
      execute_flow(Data,T,NewCache,S1);
    {match,Host,S2} -> {{match,Host}, S2};
    {need_more,S3} -> {need_more, S3}
  end.
