-module(lb).
-behaviour(application).

-export([start/2,stop/1]).

start(_Type,_StartArgs) ->
  {ok,Pid} = lb_sup:start_link(),
  case utils:get_config(tcp_enable,false) of
    true ->
      R = supervisor:start_child(
	lb_sup,
	 #{
	   id => proxy_tcp,
	   start => {proxy_tcp,start_link,[]},
	   restart => permanent,
	   shutdown => infinity,
	   type => worker,
	   modules => [proxy_tcp]
	  }
	),
      logger:info("Start child ~p",[R]);
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

  {ok,Pid}.

stop(_State) ->
  ok.
