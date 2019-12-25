-module (trait).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([analyze/1]).

-include("common.hrl").

-define(INIT_PACKET_THRESHOLD,64).


init(_Args) ->
  ML = case config:get_config() of
    {ok,Config} ->
      lists:map(fun(M) ->
        case M of
          {matcher,MP,Keyword,Backend} ->
            #matcher{
              regex_mp = MP,
              keyword = Keyword,
              addr = lists:nth(1,Backend)
            };
          {default_matcher,Timeout,Addr} ->
            #default_matcher{timeout = Timeout,addr = lists:nth(1,Addr)}
        end
      end,Config);
    error -> []
  end,

  S = #match_state{matcher_list = ML},
  {ok,S}.

start_link() ->
  io:format("trait server starting ~n"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

% match one by one, first matched one wins
handle_call({route,Data},_From,State = #match_state{matcher_list = ML}) ->
  % io:format("ML:  ~p",[ML]),
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
    {value,#matcher{addr = Addr}} -> {reply,{match,Addr},State};
    {value,#default_matcher{addr = Addr}} when S > ?INIT_PACKET_THRESHOLD
      -> {reply,{match,Addr},State}; % enough inital data to select the default matcher
    {value,#default_matcher{timeout = Timeout}} -> {reply,{again,Timeout},State}; % initial data is not enough, wait no more than Timeout
    false -> {reply,no_match,State}
  end.


handle_cast(stop,State) ->
  {stop,normal,State}.

-spec analyze(binary()) -> tuple().
analyze(Data) ->
  gen_server:call(?MODULE,{route,Data}).


code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
