-module(smartlb_udp_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
  [massive_test].

init_per_suite(Config) ->
  Path = filename:join([?config(data_dir,Config),"udp.conf"]),
  application:set_env(smartlb,flow_file,Path,[{persistent,true}]),
  application:set_env(smartlb,udp_enable,true,[{persistent,true}]),
  application:set_env(smartlb,tcp_enable,false,[{persistent,true}]),
  %% as testing in loop device, mtu is useless, enlarge it avoiding fragment
  application:set_env(smartlb,udp_mtu,2000,[{persistent,true}]),
  application:ensure_all_started(inet,temporary),
  application:ensure_all_started(smartlb,temporary),
  Config.

end_per_suite(_Config) ->
  application:stop(smartlb),
  application:stop(inet),
  ok.


massive_test(_Config) ->
  CS = create_count_server(),
  create_udp_server(6000,CS),

  TestRocket =
    [ {5000,500,200,800},
      {5001,1000,400,1200},
      {5002,1000,1200,1900}],
  Expected = launch_rocket(TestRocket),
  timer:sleep(1000),
  CS ! {info,self()},
  Expected = receive
	       {info,V} -> V
	     end,
  ok.


launch_rocket(L) ->
  Self = self(),
  lists:foreach(
    fun({Port,Iteration,Start,End}) ->
	spawn(fun() ->
		  SizeList = create_udp_client_rocket(Port,Iteration,Start,End),
		  Self ! {result,Port,Iteration,lists:sum(SizeList)}
	      end)
    end,L),
  lists:foldl(fun(_,M) ->
		  receive
		    {result,Port,Iter,Size} ->
		      maps:put(Port,{Iter,Size},M)
		  end
	      end,#{},lists:seq(1,length(L))).




create_udp_client_rocket(Port,Iteration,Start,End) ->
  {ok,LbPort} = application:get_env(smartlb,udp_port),
  {ok,Ip} = application:get_env(smartlb,udp_ip),
  {ok,S} = gen_udp:open(Port,[binary,{sndbuf,65535}]),
  Send = fun () ->
	     D = get_random_binary(Start,End),
	     ok = gen_udp:send(S,Ip,LbPort,D),
	     byte_size(D)
  end,
  [Send() || _ <- lists:seq(1,Iteration)].


create_udp_server(Port,CountServer) ->
  {ok,Socket} = gen_udp:open(Port,[binary,{active,true},{recbuf,65535},{sndbuf, 65535}]),
  Pid = spawn_link(fun() ->
		      fun Loop() ->
			  receive
			    {udp,_S,_Ip,SrcPort,Packet} ->
			      CountServer ! {count,{SrcPort,byte_size(Packet)}}
			  end,
			  Loop()
		      end()
		   end),
  ok = gen_udp:controlling_process(Socket,Pid).

create_count_server() ->
  spawn(fun() ->
	    T = ets:new(count_info,[private,set]),
	    fun Loop() ->
		receive
		  {count,{Port,Size}} ->
		    case ets:lookup(T,Port) of
		      [] -> ets:insert(T,{Port,1,Size});
		      [{_,Num,OldSize}] ->
			ets:insert(T,{Port,Num + 1,OldSize + Size})
		    end;
		  {info,Pid} ->
		    Pid ! {info,ets:foldl(fun({Port,Num,Size},M) ->
					      maps:put(Port,{Num,Size},M)
					  end,#{},T)}
		end,
		Loop()
	    end()
	end).

get_random_binary(Start,End) ->
  AllowChars="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ",
  Len = length(AllowChars),
  Num = Start + rand:uniform(End - Start),
  L = [lists:nth(rand:uniform(Len),AllowChars) || _ <- lists:seq(1,Num)],
  list_to_binary(L).
