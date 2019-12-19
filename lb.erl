-module(lb).
-behaviour(supervisor).


-export([init/1,start/0]).

start() ->
  % process_flag(trap_exit,true),
  supervisor:start_link({local,?MODULE},?MODULE,[]),
  timer:sleep(1000),
  case config:get_config() of
    {ok,Config} -> io:format("### ~p~n",[Config]);
    error -> io:format("error ...~n")
  end,
  % exit(normal),
  loop().

loop() ->
  receive
    {'EXIT',Reason,_} -> io:format("lb got exit with reason: ~p~n",[Reason]);
    _ -> io:format("lb got unknown message~n")
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