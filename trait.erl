-module (trait).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([analyze/1]).

-include("common.hrl").

-define(INIT_PACKET_THRESHOLD,8).


init(_Args) ->
  ML = case config:get_config() of
    {ok,Config} ->
      lists:map(fun(M) ->
        case M of
          {matcher,MP,Keyword,Addr} ->
            #matcher{
              regex_mp = MP,
              keyword = Keyword,
              addr = Addr
            };
          {default_matcher,Timeout,Addr} ->
            #default_matcher{timeout = Timeout,addr = Addr}
        end
      end,Config);
    error -> []
  end,

  Table = ets:new(mst,[set]),
  lists:foreach(fun(K) ->
    Address = case K of
      #matcher{addr = Addr} -> Addr;
      #default_matcher{addr = Addr} -> Addr
    end,
    ets:insert(Table,{K,#matcher_state{backend_number = length(Address)}})    % create matcher -> matcher_state mapping
  end,ML),

  S = #trait_state{matcher_list = ML, matcher_state_table = Table},
  {ok,S}.

start_link() ->
  logger:info("trait server starting ~n"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

% match one by one, first matched one wins
handle_call({route,Data},_From,State = #trait_state{matcher_list = ML,matcher_state_table = MST}) ->
  % logger:info("ML:  ~p",[ML]),
  S = byte_size(Data),
  Matched = lists:search(fun(M) ->
    case M of
      #matcher{regex_mp = MP,keyword = K} ->
        case re:run(Data,MP,[{capture,[1],list}]) of
          {match,[Key]} ->
            Key == K;
          _ -> false
        end;
      #default_matcher{addr = _Addr} -> true;

      _ -> false
    end
  end,ML),

  case Matched of
    {value,M = #matcher{}} ->
      Addr = select_backend_addr(M,MST),
      % logger:info("select addr :~p~n",[Addr]),
      {reply,{match,Addr},State};
    {value,M = #default_matcher{}} when S > ?INIT_PACKET_THRESHOLD ->
      Addr = select_backend_addr(M,MST),
      % logger:info("select addr :~p~n",[Addr]),
      {reply,{match,Addr},State}; % enough inital data to select the default matcher
    {value,#default_matcher{timeout = Timeout}} -> {reply,{again,Timeout},State}; % initial data is not enough, wait no more than Timeout
    false -> {reply,no_match,State}
  end.


handle_cast(stop,State) ->
  {stop,normal,State}.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


-spec analyze(binary()) -> tuple().
analyze(Data) ->
  gen_server:call(?MODULE,{route,Data}).

%round-robin
select_backend_addr(Matcher,Matcher_State) ->
  [{_,State = #matcher_state{index = I,backend_number = N}}] = ets:lookup(Matcher_State,Matcher),
  NewState = State#matcher_state{index = I + 1},
  ets:insert(Matcher_State,{Matcher,NewState}),    % update matcher state

  Chosen = (I rem N) + 1,
  case Matcher of
    #matcher{addr = Addr} -> lists:nth(Chosen,Addr);
    #default_matcher{addr = Addr} -> lists:nth(Chosen,Addr)
  end.
