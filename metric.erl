-module(metric).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([event/1,get_metric_data/0]).

-include("common.hrl").

init(_Args) ->
  {ok, #metric_state{}}.


start_link() ->
  logger:info("metric server starting ~n"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_cast(Event,State) ->
  NewState = case Event of
    incoming_conn ->
      #metric_state{incoming_conn_total_number = N} = State,
      State#metric_state{incoming_conn_total_number = N + 1};
    incoming_conn_fail ->
      #metric_state{incoming_conn_failed_number = N} = State,
      State#metric_state{incoming_conn_failed_number = N + 1};
    _ -> State
  end,
  {noreply,NewState}.

handle_call(_Cmd,_From,State) ->
  {reply,State,State}.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


event(Event) ->
  gen_server:cast(?MODULE,Event).

get_metric_data() ->
  gen_server:call(?MODULE,data).
