-module(lb_http).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).

-include("common.hrl").

init(State) ->
  Port = utils:get_config(http_port,?HTTP_PORT),
  Dispatch = cowboy_router:compile([
				    {'_', [{'_', http_handler, []}
					  ]}
				   ]),
  case cowboy:start_clear(http_listener,
		     [{port, Port}],
		     #{env => #{dispatch => Dispatch}}
		    ) of
    {ok,_} -> {ok,State};
    {error,Reason} -> {error,Reason}
  end.


start_link() ->
  logger:info("http server starting"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast(_Request,State) ->
  {noreply,State}.
