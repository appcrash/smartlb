-module(config).
-behaviour(gen_server).
-export([init/1,start_link/0,handle_call/3,handle_cast/2,code_change/3]).
-export([get_config/0]).
-define(CONFIG_FILE,"lb.conf").

init(_Args) ->
  {ok,[]}.

start_link() ->
  logger:info("config server starting ~n"),
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).


handle_call(get_config,_From,State) ->
  {reply,read_config(),State}.

handle_cast(stop,State) ->
  {stop,normal,State}.

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.


get_config() ->
  gen_server:call(?MODULE,get_config).


% {ok,Terms} || error
read_config() ->
  case file:consult(?CONFIG_FILE) of
    {ok,Terms} ->
      case check_config(Terms) of
        ok -> {ok,Terms};
        _ -> error
      end;
    {error,Reason} ->
      logger:error("read config file error: ~p~n",[Reason]),
      error
  end.


% check config return ok || error

check_config([]) -> ok;
check_config([H|T]) ->
  Re = case H of
    {matcher,MP,Keyword,Backend} -> check_item_matcher({MP,Keyword,Backend});
    {default_matcher,Timeout,Backend} -> check_item_matcher_backend(Backend);
    Any -> {error,Any}
  end,

  case Re of
    {error,Reason} ->
      logger:error("check config item error: ~p~n",[Reason]),
      error;
    ok -> check_config(T)
  end;
check_config(_) -> error.

% check_item_* return error reason || ok
check_item_matcher({MP,_Keyword,Backend}) ->
  case re:compile(MP) of
    {error,Reason} -> Reason;
    {ok,_} -> check_item_matcher_backend(Backend)
  end;
check_item_matcher(_) -> "unknown item".


check_item_matcher_backend([]) -> ok;
check_item_matcher_backend([{Ip,Port}|T]) when is_integer(Port) ->
  case inet:parse_ipv4_address(Ip) of
    {error,Reason} -> Reason;
    {ok,_} -> check_item_matcher_backend(T)
  end;
check_item_matcher_backend(_) -> "unknown matcher backend".
