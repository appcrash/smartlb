-module(lb).
%-behaviour(supervisor).
-behaviour(application).

-export([init/1,start/2,stop/1]).

start(_Type,_StartArgs) ->
  % process_flag(trap_exit,true),
  % supervisor:start_link({local,?MODULE},?MODULE,[]),
  {ok,Pid} = lb_sup:start_link(),
  % timer:sleep(1000),
  case config:get_config() of
    {ok,Config} ->
      logger:info("######~n ~p~n######",[Config]),
      {ok,Pid};
    error ->
      logger:error("config error, shutdown ..."),
      exit(shutdown)
  end.


stop(_State) ->
  ok.


init(_) ->
  {ok, {{one_for_one, 3 ,10},
    [
      {lb_sup,                   %tag
        {lb_sup,start_link,[]},  %mfa
        permanent,   % restart
        infinity,        % terminate timeout
        supervisor,      % type
        [lb_sup]      % release handling
      }
    ]
  }}.
