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
      logger:info("######~n ~p~n######~n",[Config]),
      {ok,Pid};
    error ->
      logger:error("config error, shutdown ... ~n"),
      exit(shutdown)
  end.

  %loop().

stop(_State) ->
  ok.

loop() ->
  receive
    {'EXIT',Reason,_} -> logger:error("lb got exit with reason: ~p~n",[Reason]);
    _ -> logger:info("lb got unknown message~n")
  end,
  loop().



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
