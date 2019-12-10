#!/usr/bin/env escript


-module(proxy).
-behaviour(gen_server).

-export([main/1,init/1,handle_cast/2,handle_call/3,code_change/3]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, true}, {reuseaddr, true}]).


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
          io:format("listen ok~n"),
          {ok, accept_loop(NewState)};

      {error, Reason} -> {stop, Reason}
  end.

accept_loop(_State) ->
  % #server_state{port = Port} = State,
  io:format("port is ~n").


handle_cast(stop,State) ->
  {stop,normal,State}.


handle_call(_Msg, _Caller, State) -> {noreply, State}.


code_change(_OldVersion, Library, _Extra) -> {ok, Library}.
