-module(metric).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([event/1,get_metric_data/0]).

-include("common.hrl").


%% use metric:event(EventName) to increase the metric in the server state
init(_Args) ->
  ets:new(metric_info,[named_table,set,private]),
  {ok, #{}}.


start_link() ->
  logger:info("metric server starting"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_cast({count,Key},State) ->
  case ets:member(metric_info,Key) of
    true ->
      ets:update_counter(metric_info,Key,{2,1});
    false ->
      ets:insert(metric_info,{Key,1})
  end,
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.

handle_call(_Cmd,_From,State) ->
  R = ets:foldl(
	fun({K,V},Map) -> maps:put(K,V,Map) end,
	#{},metric_info),
  {reply,R,State}.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


event(Event) ->
  gen_server:cast(?MODULE,{count,Event}).

get_metric_data() ->
  gen_server:call(?MODULE,data).
