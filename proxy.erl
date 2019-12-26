-module(proxy).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_cast/2,handle_call/3,code_change/3,terminate/2]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false},
 {reuseaddr, true}, {nodelay, true}]).
-define(TCP_CONN_OPTIONS,[binary, {packet, 0}, {active, false},
 {reuseaddr, true}, {nodelay, true}]).
-define(SRC_PORT,7777).
-define(PREFORK,1).

-record(server_state,{
  port,
  listen_socket = null
}).


start_link() ->
  logger:info("proxy server starting ~n"),
  State = #server_state{port = ?SRC_PORT},
  gen_server:start_link({local,?MODULE},?MODULE,State,[]).


init(State) ->
  process_flag(trap_exit,true),
  #server_state{port = Port} = State,
  case gen_tcp:listen(Port,?TCP_OPTIONS) of
      {ok, Listen_Socket} ->
          NewState = State#server_state{listen_socket = Listen_Socket},
          prefork(?PREFORK,Listen_Socket),
          {ok, NewState};

      {error, Reason} ->
        logger:error("proxy can not create listen socket: ~p~n",[Reason]),
        {stop, Reason}
  end.


terminate(Reason,_State = #server_state{listen_socket = LS}) ->
  logger:error("proxy terminate: ~p~n",[Reason]),
  gen_tcp:close(LS).

code_change(_OldVersion, Library, _Extra) -> {ok, Library}.

handle_cast(stop,State) ->
  {stop,normal,State}.

handle_call(accepted, _From, State = #server_state{listen_socket = LS}) ->
  Pid = spawn(fun() -> accept(LS) end),
  {reply,Pid,State}.

prefork(Remaining,LS) ->
  if
    Remaining > 0 ->
      spawn(fun() -> accept(LS) end),
      prefork(Remaining - 1,LS);
    true -> ok
  end.


accept(Listen_Socket) ->
  % logger:info("new proxy accept process~n"),
  A = gen_tcp:accept(Listen_Socket),
  gen_server:call(?MODULE,accepted),
  case A of
    {ok,Socket} -> process_socket(Socket);
    {error,Reason} -> logger:error("accept error ~p~n",[Reason])
  end.


process_socket(Socket) ->
  inet:setopts(Socket,[{nopush, false}]),
  case analyze_trait(Socket,<<>>) of
    error ->
      logger:info("socket stream has no trait, close it ~n"),
      gen_tcp:close(Socket);
    {ok,{Ip,Port},Buffered_Packet} ->
      % logger:info("selected: ~p:~p~n",[Ip,Port]),
      % logger:info("~p~n^^^^send buffered data^^^~n",[binary_to_list(Buffered_Packet)]),
      case gen_tcp:connect(Ip,Port,?TCP_CONN_OPTIONS) of
        {ok,To_Socket} ->
          P1 = spawn(fun() -> socket_loop(Socket) end),
          P2 = spawn(fun() -> socket_loop(To_Socket) end),

          gen_tcp:controlling_process(Socket,P1),
          gen_tcp:controlling_process(To_Socket,P2),

          P1 ! {ready,P2},
          P2 ! {ready_buffered,P1,Buffered_Packet};  % send buffered packet to destination when connected

        {error,Reason} ->
          logger:error("connect dest ip with error ~p~n",[Reason]),
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
      logger:error("tcp error with reason: ~p~n",[Reason]),
      gen_tcp:close(Socket),
      Pid ! {peer_closed};

    {send,Packet} ->
      % logger:info("~p~n^^^^^send data^^^^^~n",[binary_to_list(Packet)]),
      gen_tcp:send(Socket,Packet),
      socket_loop(Socket,Pid);

    {peer_closed} -> gen_tcp:close(Socket)
  end.


% receive some bytes from incoming socket, analyze it to determine routing strategy
analyze_trait(Socket,<<>>) ->
  case gen_tcp:recv(Socket,0) of      % receive first packet without timeout
    {ok,Packet} -> analyze_trait(Socket,Packet);
    {error,_} -> error
  end;
analyze_trait(Socket,Data) ->
  % logger:info("analyzing:~n~p~n^^^^analyzed^^^^n",[binary_to_list(Data)]),
  case trait:analyze(Data) of
    {match,Addr} -> {ok,Addr,Data};
    {again,Timeout} ->
      case gen_tcp:recv(Socket,0,Timeout) of
        {ok,Packet} -> analyze_trait(Socket,<<Data/binary,Packet/binary>>);
        {error,timeout} ->
          logger:info("incoming connection has not enough data within timeout value, close the socket~n"),
          error;
        {error,_Reason} -> error
      end;
    no_match -> error
  end.
