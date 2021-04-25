-module(lb).
-behaviour(application).

-export([start/2,stop/1]).

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
      logger:info("tcp server not enabled)")
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
      logger:info("udp server not enabled)")
  end,

  {ok,Terms} = file:script("lb.conf"),
  FlowFuncs = matcher_builder:build(Terms),
  logger:info("FlowFuncs Terms ~p",[Terms]),
  matcher_master:set_config(FlowFuncs),

  {ok,Pid}.

stop(_State) ->
  ok.
