-module(lb_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).


init(_) ->
  {ok, {{one_for_one, 3 ,10},
    [
      {trait,                   %tag
        {trait,start_link,[]},  %mfa
        permanent,   % restart
        5000,        % terminate timeout
        worker,      % type
        [trait]      % release handling
      },
      {proxy,
        {proxy,start_link,[]},
        permanent,
        5000,
        worker,
        [proxy]
      }
    ]
  }}.