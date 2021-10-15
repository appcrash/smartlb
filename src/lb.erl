-module(lb).
-behaviour(application).

-export([start/2,stop/1,reload_config/0]).

start(_Type,_StartArgs) ->
  logger:add_handler_filter(default,progress,
   			    {fun logger_filters:progress/2,stop}),
  {ok,Pid} = lb_sup:start_link(),
  case utils:get_config(tcp_enable,false) of
    true ->
      supervisor:start_child(
	lb_sup,
	 #{
	   id => proxy_tcp,
	   start => {proxy_tcp,start_link,[]},
	   restart => permanent,
	   shutdown => infinity,
	   type => worker,
	   modules => [proxy_tcp]
	  }
	);
    _ ->
      logger:info("tcp server not enabled")
  end,

  case utils:get_config(udp_enable,false) of
    true ->
      supervisor:start_child(
	lb_sup,
	#{
	  id => proxy_udp_sup,
	  start => {proxy_udp_sup,start_link,[]},
	  restart => permanent,
	  shutdown => infinity,
	  type => supervisor,
	  modules => [proxy_udp_sup]
	 }
       );
    _ ->
      logger:info("udp server not enabled")
  end,

  case reload_config() of
    ok -> {ok,Pid};
    {error,Reason} -> {error,Reason}
  end.

stop(_State) ->
  ok.


-spec reload_config() -> ok | {error,string()}.
reload_config() ->
  FlowFile = utils:get_config(flow_file,"lb.conf"),
  case file:script(FlowFile) of
    {ok,Terms} ->
      FlowFuncs = matcher_builder:build(Terms),
      logger:info("FlowFuncs Terms ~p",[Terms]),
      matcher_master:set_config(FlowFuncs),
      ok;
    {error,Reason} when is_tuple(Reason) ->
      Desc = file:format_error(Reason),
      {error,Desc};
    {error,_} -> {error,"Read config file failed"}
  end.
