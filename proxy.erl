-module(proxy).
-behaviour(gen_server).

-export([main/1,init/1,handle_cast/2,handle_call/3,code_change/3]).

-define(TCP_OPTIONS, [list, {packet, 0}, {active, false}, {reuseaddr, true}]).
-define(SRC_PORT,7777).
-define(DEST_PORT,9999).

-record(server_state,{
  port,
  listen_socket = null
}).


main(_Args) ->
  io:format("proxy starting ...~n"),
  start_proxy(smart_lb,?SRC_PORT).

start_proxy(Name,Port) ->
  State = #server_state{port = Port},
  gen_server:start_link({local,Name},?MODULE,State,[]).

init(State) ->
  #server_state{port = Port} = State,
  case gen_tcp:listen(Port,?TCP_OPTIONS) of
      {ok, Listen_Socket} ->
          NewState = State#server_state{listen_socket = Listen_Socket},
          Pid = spawn(fun() -> keep_process_loop(4,Listen_Socket) end),
          register(kpl,Pid),
          {ok, NewState};

      {error, Reason} -> {stop, Reason}
  end.


keep_process_loop(Remaining,Listen_Socket) when Remaining > 0 ->
  io:format("keep process loop remaining ~p~n",[Remaining]),
  spawn(fun() -> accept(Listen_Socket) end),
  keep_process_loop(Remaining - 1,Listen_Socket);
keep_process_loop(0,Listen_Socket) ->
  receive
    {accept_finished} -> keep_process_loop(1,Listen_Socket)
  end.


accept(Listen_Socket) ->
  case gen_tcp:accept(Listen_Socket) of
    {ok,Socket} -> process_socket(Socket);
    {error,Reason} -> io:format("accept error ~p~n",[Reason])
  end,
  kpl ! {accept_finished}.




socket_loop(Socket) ->
  receive
    {ready,Pid} ->
      inet:setopts(Socket,[{active,true}]),
      socket_loop(Socket,Pid)
  end.
socket_loop(Socket,Pid) ->
  receive
    {tcp,Socket,Packet} ->
      Pid ! {send,Packet},
      socket_loop(Socket,Pid);

    {send,Packet} ->
      gen_tcp:send(Socket,Packet),
      socket_loop(Socket,Pid);

    {tcp_closed,_} ->
      Pid ! {peer_closed};

    {peer_closed} -> gen_tcp:close(Socket)
  end.

process_socket(Socket) ->
  case gen_tcp:connect("localhost",?DEST_PORT,?TCP_OPTIONS) of
    {ok,To_Socket} ->
      P1 = spawn(fun() -> socket_loop(Socket) end),
      P2 = spawn(fun() -> socket_loop(To_Socket) end),

      gen_tcp:controlling_process(Socket,P1),
      gen_tcp:controlling_process(To_Socket,P2),

      P1 ! {ready,P2},
      P2 ! {ready,P1};

    {error,Reason} ->
      io:format("connect dest ip with error ~p~n",[Reason]),
      gen_tcp:close(Socket)
  end.



handle_cast(stop,State) ->
  {stop,normal,State}.


handle_call(_Msg, _Caller, State) -> {noreply, State}.


code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
