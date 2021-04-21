-module(matcher).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([set_config/1,match/1]).
-include("common.hrl").


-type id() :: string() | atom().

-record(matcher_item,
{
 id :: id(),
 match_func :: function()
}).
-record(backend_item,
{
 id :: id(),
 strategy :: string(),
 host :: [tuple()]
}).
-record(rule_item,
{
 id :: id(),
 matcher_id :: string(),
 rule_func :: function()
}).
-record(flow_item,
{
 match_rule :: id(),
 target :: tuple()
}).


init(_Args) ->
  MT = ets:new(matcher,[set,{keypos,#matcher_item.id}]),
  BT = ets:new(backend,[set,{keypos,#backend_item.id}]),
  RT = ets:new(rule,[set,{keypos,#rule_item.id}]),
  {ok,#{matcher => MT,backend => BT,rule => RT}}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).


handle_call({set_config,Terms},_From,State) ->
  NewState=build_terms(Terms,State),
  {reply,ok,NewState};
handle_call({match,Data},_From,#{flow:=FlowFuncs}=State) ->
  R = execute_flow(Data,FlowFuncs),
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast(_Request,State) ->
  {noreply,State}.


%% ################# API ##################
-spec set_config([term()]) -> ok | error.
set_config(Terms) ->
  gen_server:call(?MODULE,{set_config,Terms}).

-spec match(binary) -> {match,tuple()} | nomatch.
match(Data) ->
  gen_server:call(?MODULE,{match,Data}).
%% ################# API ##################



build_terms(Terms,#{matcher:=MT,backend:=BT,rule:=RT}=State) ->
  %% basic transformation, sanity check
  ALL = lists:flatmap(
    fun(E) ->
	case E of
	  {matcher,M} -> build_matcher(M);
	  {backend,B} -> build_backend(B);
	  {rule,R} -> build_rule(R);
	  {flow,F} -> build_flow(F)
	end
    end, Terms),
  %% no error throws, so update info into ets table
  %% get flow_items out and put other items to table
  %% as flow needs more complex handling
  Flows = lists:foldl(
	    fun(E,Acc) ->
		case E of
		  M when is_record(M,matcher_item) ->
		    ets:insert(MT,M),
		    Acc;
		  B when is_record(B,backend_item) ->
		    ets:insert(BT,B),
		    Acc;
		  R when is_record(R,rule_item) ->
		    ets:insert(RT,R),
		    Acc;
		  F when is_record(F,flow_item) ->
		    [F | Acc]
		end
	    end,[],ALL),
  %% important!!! keep the rule order as config file, remember to reverse
  FlowFuncs = compose_flow(lists:reverse(Flows),State),
  State#{flow=>FlowFuncs}.

build_matcher(M) ->
  lists:map(
    fun(#{id := Id,type := Type} = E) ->
	case Type of
	  "regex" ->
	    #{pattern := Pattern,capture := Capture} = E,
	    {ok,MP} = re:compile(Pattern),
	    MatchFunc =
	      fun(Data) ->
		  re:run(Data,MP,[{capture,Capture,list}])
	      end,
	    #matcher_item{id=Id,match_func=MatchFunc};
	  "func" ->
	    #{function := F} = E,
	    case is_function(F,1) of
	      true -> ok;
	      _ -> error(wrong_matcher_func)
	    end,
	    #matcher_item{id=Id,match_func=F}
	end
    end,M).

build_backend(B) ->
  lists:map(
    fun(#{id := Id,strategy := Strt,host := Host}) ->
	#backend_item{id=Id,strategy=Strt,host=Host}
    end,B).

build_rule(R) ->
  lists:map(
    fun(#{id := Id,matcher_id := Mid,condition := F}) ->
	#rule_item{id=Id,matcher_id=Mid,rule_func=F}
    end,R).

build_flow(F) ->
  lists:map(
    fun(#{match_rule := Mr,target := Target}) ->
	#flow_item{match_rule=Mr,target=Target}
    end,F).

%% connect matcher,rule and backend by building function
%% defined by flow item info, the sanity check is performed by the way
compose_flow(#flow_item{match_rule=RuleId,target=Target}=Flow,
	     #{matcher:=MT,backend:=BT,rule:=RT}) when is_record(Flow,flow_item) ->
  [#rule_item{id=RuleId,matcher_id=Mid,rule_func=RuleFunc}] = ets:lookup(RT,RuleId),
  [#matcher_item{match_func=MatchFunc}] = ets:lookup(MT,Mid),
  case Target of
    {backend,Bid} ->
      [#backend_item{host=Host}] = ets:lookup(BT,Bid)
  end,

  %% build the rule function
  fun(Data,MatchCacheMap) ->
      %% search match cache before invoke match_func
      {MatchResult,NewCacheMap} =
	case maps:get(Mid,MatchCacheMap,none) of
	  none ->
	    case MatchFunc(Data) of
	      {match,Matched} ->
		{Matched,maps:put(Mid,Matched,MatchCacheMap)};
	      nomatch ->
		{nomatch,maps:put(Mid,nomatch,MatchCacheMap)}
	    end;
	  Cache -> {Cache,MatchCacheMap}
	end,
      %% match result it definite and cached now
      case MatchResult of
	nomatch -> {nomatch,NewCacheMap};
	MatchedList ->
	  case RuleFunc(MatchedList) of
	    true ->
	      {match,Host};
	    _ -> {nomatch,NewCacheMap}
	  end
      end
  end;
compose_flow(Flows,State) ->
  lists:map(fun(F) -> compose_flow(F,State) end,Flows).

-spec execute_flow(binary(),[function()]) -> {match,tuple()} | nomatch.
execute_flow(Data,FlowFuncs) ->
  execute_flow(Data,FlowFuncs,#{}).
execute_flow(_,[],_) -> nomatch;
execute_flow(Data,[H|T],Cache) ->
  case H(Data,Cache) of
    {nomatch,NewCache} ->
      execute_flow(Data,T,NewCache);
    {match,Host} -> {match,Host}
  end.
