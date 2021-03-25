-module(proxy_udp_sup).
-behaviour(supervisor).

-export([start_link/0,init/1]).


start_link() ->
  supervisor:start_link({local,?MODULE},?MODULE,[]).

init(_Args) ->
  SupFlags = #{strategy => one_for_one,
	      intensity => 1,
	      period => 5},
  ChildSpecs = [
    #{id => proxy_udp_receive,
      start => {proxy_udp_receive, start_link, []},
      restart => permanent,
      shutdown => 10000,
      type => worker
      }

  ],

  {ok, {SupFlags, ChildSpecs}}.
