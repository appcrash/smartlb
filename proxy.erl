-module(proxy).
-behaviour(gen_server).

-export([main/1,init/1,handle_cast/2,handle_call/3,code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).


-record(server_state,{
  port,
  listen_socket = null
}).


main(_Args) ->
  io:format("proxy starting ...~n"),
  start_proxy(smart_lb,7777).

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

  

process_socket(Socket) ->
  case gen_tcp:recv(Socket,16) of
    {ok,Packet} ->
      io:format("packet is ~p",[Packet]),
      process_socket(Socket);
    {error,Reason} -> io:format("accept error ~p~n",[Reason])
  end.


  
  
handle_cast(stop,State) ->
  {stop,normal,State}.


handle_call(_Msg, _Caller, State) -> {noreply, State}.


code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
