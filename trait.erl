-module (trait).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([analyze/1]).


-record(matcher,{
  regex_mp,
  keyword,
  addr
}).

-record(state,{
  matcher_list
}).


init(_Args) ->
  {_,MP1} = re:compile("(?i)service-provider:\\s*(\\w+)\\s*\r\n"),
  {_,MP2} = re:compile("(?i)service-provider:\\s*(\\w+)\\s*\r\n"),
  State = #state{
    matcher_list = [
      #matcher{
        regex_mp = MP1,
        keyword = "xf",
        addr ={"127.0.0.1",18000}
      },
      #matcher{
        regex_mp = MP2,
        keyword = "fx",
        addr = {"127.0.0.1",19000}
      }
    ]
  },

  {ok,State}.

start_link() ->
  io:format("trait server starting ~n"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

% match one by one, first matched one wins
handle_call({route,Data},_From,State = #state{matcher_list = ML}) ->
  % io:format("ML:  ~p",[ML]),
  Matched = lists:search(fun(#matcher{regex_mp = MP,keyword = K}) ->
    case re:run(Data,MP,[{capture,[1],list}]) of
      {match,[Key]} ->
        Key == K;
      _ -> false
    end
  end,ML),
  case Matched of
    {value,#matcher{addr = Addr}} -> {reply,{match,Addr},State};
    false -> {reply,no_match,State}
  end;
handle_call({route1,Data},_From,State = #state{matcher_list = ML}) ->
  io:format("eeee~n"),
  io:format("~p",Data).

handle_cast(stop,State) ->
  {stop,normal,State}.

-spec analyze(binary()) -> tuple().
analyze(Data) ->
  gen_server:call(?MODULE,{route,Data}).


code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
