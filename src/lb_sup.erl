-module(lb_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).


init(_) ->
  {ok, {{one_for_one, 3 ,10},
    [
     {metric,
       {metric,start_link,[]},
       permanent,
       5000,
       worker,
       [metric]
     },
     {lb_http,
        {lb_http,start_link,[]},
        permanent,
        5000,
        worker,
        [lb_http]
      },

      {matcher_master,
        {matcher_master,start_link,[]},
        permanent,
        5000,
        worker,
        [matcher_master]
      }

    ]
  }}.
