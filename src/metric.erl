-module(metric).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([event/1,get_metric_data/0]).

-include("common.hrl").


%% use metric:event(EventName) to increase the metric in the server state
init(_Args) ->
  init_http(),
  {ok, #{
    incoming_conn => 0,
    incoming_conn_fail => 0,
    analyze_trait_timeout => 0
  }}.


init_http() ->
  Port = utils:get_config(metric_http_port,?METRIC_HTTP_PORT),
  Dispatch = cowboy_router:compile([
				    {'_', [{"/metric", metric_handler, []}]}
				   ]),
  cowboy:start_clear(metric_http_listener,
			       [{port, Port}],
			       #{env => #{dispatch => Dispatch}}
			      ).

start_link() ->
  logger:info("metric server starting"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_cast(Event,State) ->
  NewState = case Event of
    {count,Key} ->
      try maps:update_with(Key,fun(V) -> V + 1 end,State)
      catch
	error:{badkey,K} -> logger:error("metric server count a wrong key ~p~n",[K]),State;
	_:_ -> logger:error("metric server got a unknown exception"),State
      end;
    _ -> State
  end,
  {noreply,NewState}.

handle_call(_Cmd,_From,State) ->
  {reply,State,State}.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


event(Event) ->
  gen_server:cast(?MODULE,{count,Event}).

get_metric_data() ->
  gen_server:call(?MODULE,data).
