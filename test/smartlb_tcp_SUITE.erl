-module(smartlb_tcp_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

-define(TCP_LISTEN_OPTIONS, [binary, {packet, 0}, {active, true},
 {reuseaddr, true}, {nodelay, true}]).
-define(TCP_CONN_OPTIONS,[binary, {packet, 0}, {active, false},
 {reuseaddr, true}, {nodelay, true}]).

-define(EXPECT_RECEIVE(Key),
	fun() ->
	    receive
	      {Key,Value} -> Value
	    after
	      1000 -> timeout
	    end
	end()).



all() ->
  [match_test,need_more_test].

init_per_suite(Config) ->
  Path = filename:join([?config(data_dir,Config),"tcp.conf"]),
  application:set_env(smartlb,flow_file,Path,[{persistent,true}]),
  application:set_env(smartlb,udp_enable,false,[{persistent,true}]),
  application:set_env(smartlb,tcp_enable,true,[{persistent,true}]),
  application:ensure_all_started(inet,temporary),
  application:ensure_all_started(smartlb,temporary),
  Config.

end_per_suite(_Config) ->
  application:stop(smartlb),
  application:stop(inet),
  ok.


match_test(_Config) ->
  create_echo_server(self(),6001,match),
  create_echo_server(self(),6000,default),

  S1 = connect_to_lb(),
  DataMatch = <<"oewfjwewkds service-provider: google \r\nsdlifjwweif">>,
  gen_tcp:send(S1,DataMatch),
  DataMatch = ?EXPECT_RECEIVE(match),
  S2 = connect_to_lb(),
  DataNoMatch = <<"some data that would not be matched">>,
  gen_tcp:send(S2,DataNoMatch),
  DataNoMatch = ?EXPECT_RECEIVE(default),
  S3 = connect_to_lb(),
  DataWrongSp = <<" sffu service-provider: not_google \r\n iwoef\r\nioew">>,
  gen_tcp:send(S3,DataWrongSp),
  DataWrongSp = ?EXPECT_RECEIVE(default),
  [gen_tcp:close(S) || S <- [S1,S2,S3]],
  ok.

need_more_test(_Config) ->
  create_echo_server(self(),6000,default),
  S = connect_to_lb(),
  Data1 = <<"small data">>,
  Data2 = <<"more data, big enough to get match result">>,
  gen_tcp:send(S,Data1),
  timeout = ?EXPECT_RECEIVE(default),
  gen_tcp:send(S,Data2),
  D =  <<Data1/binary,Data2/binary>>,
  D = ?EXPECT_RECEIVE(default),
  Data3 = <<"even more data">>,
  gen_tcp:send(S,Data3),
  Data3 = ?EXPECT_RECEIVE(default),
  gen_tcp:close(S),
  ok.

connect_to_lb() ->
  {ok,LbPort} = application:get_env(smartlb,tcp_port),
  {ok,Ip} = application:get_env(smartlb,tcp_ip),
  {ok,S} = gen_tcp:connect(Ip,LbPort,?TCP_CONN_OPTIONS),
  S.

create_echo_server(Pid,Port,EchoKey) ->
  spawn_link(
    fun() ->
  	{ok,L} = gen_tcp:listen(Port,?TCP_LISTEN_OPTIONS),
  	accept_loop(L,fun(S) -> echo_loop(Pid,S,EchoKey) end)
    end).

accept_loop(L,Func) ->
  {ok,S} = gen_tcp:accept(L),
  Pid = spawn(fun() -> Func(S) end),
  gen_tcp:controlling_process(S,Pid),
  accept_loop(L,Func).

echo_loop(Pid,S,EchoKey) ->
  receive
    {tcp,_,Data} ->
      Pid ! {EchoKey,Data},
      echo_loop(Pid,S,EchoKey);
    {tcp_closed,_} ->
      ok
  end.
