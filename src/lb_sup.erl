-module(lb_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).


init(_) ->
  {ok, {{one_for_one, 3 ,10},
    [
      {config,
        {config,start_link,[]},
        permanent,
        5000,
        worker,
        [config]
      },
      {metric,
        {metric,start_link,[]},
        permanent,
        5000,
        worker,
        [metric]
      },
      {matcher_master,
        {matcher_master,start_link,[]},
        permanent,
        5000,
        worker,
        [matcher_master]
      },

      {trait,                   %tag
        {trait,start_link,[]},  %mfa
        permanent,   % restart
        5000,        % terminate timeout
        worker,      % type
        [trait]      % release handling
      },
      {proxy_tcp,
        {proxy_tcp,start_link,[]},
        permanent,
        5000,
        worker,
        [proxy_tcp]
      },
      {proxy_udp_sup,
        {proxy_udp_sup,start_link,[]},
        permanent,
        5000,
        supervisor,
        [proxy_udp_sup]
      }


    ]
  }}.
