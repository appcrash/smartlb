-module(proxy_udp_receive).

-export([start_link/0]).

-include("common.hrl").

-define(UDP_LISTEN_OPTIONS, [binary, {active, false}, {recbuf,65535}, {sndbuf, 65535}]).

%% simple udp receiving loop which is supervised
%% receive as quickly as process can to avoid socket buffer running out
start_link() ->
  proxy_udp_forward:start_link(),
  Pid = spawn_link(fun() -> receive_udp() end),
  {ok,Pid}.


receive_udp() ->
  Port = utils:get_config(udp_port,?UDP_SRC_PORT),
  Ip = utils:get_config(udp_ip,?UDP_SRC_IP),
  case inet:parse_address(Ip) of
    {ok,Address} ->
      UdpOption = ?UDP_LISTEN_OPTIONS ++ [{ip, Address}];
    {error,_} ->
      logger:error("proxy udp receiver: wrong listen address, use default(listen to all address)"),
      UdpOption = ?UDP_LISTEN_OPTIONS
  end,

  case gen_udp:open(Port,UdpOption) of
    {ok, Socket} ->
      loop(Socket);
    _ -> logger:error("udp proxy receiver bind address failed")
  end.


loop(Socket) ->
  case gen_udp:recv(Socket,0) of
    {ok,Data} ->
      proxy_udp_forward:forward(Data);
    {error, Reason} ->
      logger:error("udp proxy receiver error: ~p",[Reason])
  end,
  loop(Socket).
