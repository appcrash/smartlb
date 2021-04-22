-module(proxy_tcp).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3,code_change/3,terminate/2]).

-include("common.hrl").

-define(TCP_LISTEN_OPTIONS, [binary, {packet, 0}, {active, false},
 {reuseaddr, true}, {nodelay, true},{backlog,65535}]).
-define(TCP_CONN_OPTIONS,[binary, {packet, 0}, {active, false},
 {reuseaddr, true}, {nodelay, true}]).


-record(server_state,
{
 port,
 init_packet_timeout,
 listen_socket = null
}).


start_link() ->
  Port = utils:get_config(tcp_port,?TCP_SRC_PORT),
  Timeout = utils:get_config(tcp_init_timeout,?TCP_INIT_TIMEOUT),
  State = #server_state{port = Port,init_packet_timeout=Timeout},
  gen_server:start_link({local,?MODULE},?MODULE,State,[]).


init(State) ->
  process_flag(trap_exit,true),
  #server_state{port = Port} = State,
  case gen_tcp:listen(Port,?TCP_LISTEN_OPTIONS) of
      {ok, Listen_Socket} ->
          NewState = State#server_state{listen_socket = Listen_Socket},
          prefork(?PREFORK,Listen_Socket,State),
          {ok, NewState};

      {error, Reason} ->
        logger:error("proxy can not create listen socket: ~p",[Reason]),
        {stop, Reason}
  end.


terminate(Reason,_State = #server_state{listen_socket = LS}) ->
  logger:error("proxy terminate: ~p",[Reason]),
  gen_tcp:close(LS).

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

handle_cast(stop,State) ->
  {stop,normal,State}.

handle_call(accepted, _From, State = #server_state{listen_socket = LS}) ->
  Pid = spawn(fun() -> accept(LS,State) end),
  {reply,Pid,State}.

prefork(Remaining,LS,State) ->
  if
    Remaining > 0 ->
      spawn(fun() -> accept(LS,State) end),
      prefork(Remaining - 1,LS,State);
    true -> ok
  end.


accept(Listen_Socket,State) ->
  % logger:info("new proxy accept process"),
  A = gen_tcp:accept(Listen_Socket),
  gen_server:call(?MODULE,accepted),
  metric:event(incoming_conn),
  case A of
    {ok,Socket} -> process_socket(Socket,State);
    {error,Reason} -> logger:error("accept error ~p",[Reason])
  end.


process_socket(Socket,State) ->
  inet:setopts(Socket,[{nopush, false}]),
  case analyze_trait(Socket,<<>>,State) of
    error ->
      logger:info("socket stream has no trait, close it"),
      metric:event(incoming_conn_fail),
      gen_tcp:close(Socket);
    {ok,{Ip,Port},Buffered_Packet} ->
      case gen_tcp:connect(Ip,Port,?TCP_CONN_OPTIONS) of
        {ok,To_Socket} ->
          P1 = spawn(fun() -> socket_loop(Socket) end),
          P2 = spawn(fun() -> socket_loop(To_Socket) end),

          gen_tcp:controlling_process(Socket,P1),
          gen_tcp:controlling_process(To_Socket,P2),

          P1 ! {ready,P2},
          P2 ! {ready_buffered,P1,Buffered_Packet};  % send buffered packet to destination when connected

        {error,Reason} ->
          logger:error("connect dest ip with error ~p",[Reason]),
          gen_tcp:close(Socket)
      end
  end.


socket_loop(Socket) ->
  receive
    {ready,Pid} ->
      inet:setopts(Socket,[{active,true}]),
      socket_loop(Socket,Pid);
    {ready_buffered,Pid,Buffered_Packet} ->
      gen_tcp:send(Socket,Buffered_Packet),
      inet:setopts(Socket,[{active,true}]),
      socket_loop(Socket,Pid)
  end.
socket_loop(Socket,Pid) ->
  receive
    {tcp,Socket,Packet} ->
      Pid ! {send,Packet},
      socket_loop(Socket,Pid);

    {tcp_closed,_} ->
      Pid ! {peer_closed};

    {tcp_error, _Socket, Reason} ->
      logger:error("tcp error with reason: ~p",[Reason]),
      gen_tcp:close(Socket),
      Pid ! {peer_closed};

    {send,Packet} ->
      gen_tcp:send(Socket,Packet),
      socket_loop(Socket,Pid);

    {peer_closed} -> gen_tcp:close(Socket)
  end.

%% receive some bytes from incoming socket, analyze it to determine routing strategy
analyze_trait(Socket,<<>>,State) ->
  case gen_tcp:recv(Socket,0) of      % receive first packet without timeout
    {ok,Packet} -> analyze_trait(Socket,Packet,State);
    {error,_} -> error
  end;
analyze_trait(Socket,Data,#server_state{init_packet_timeout=Timeout}=State) ->
  case matcher_master:match(Data) of
    {match,Addr} -> {ok,Addr,Data};
    need_more -> % initial packet proves nomatch with enough data
      %% let this connection accumulates more initial data in chance of being matched
      case gen_tcp:recv(Socket,0,Timeout) of
	{ok,Packet} -> analyze_trait(Socket,<<Data/binary,Packet/binary>>,State);
	{error,timeout} ->
	  logger:info("incoming connection has not enough data within timeout value, close the socket"),
	  metric:event(analyze_trait_timeout),
	  error;
	{error,_Reason} -> error
      end;
    nomatch -> error
  end.
