-module(matcher_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
  [basic_test,cache_test].



init_per_testcase(_Case,Config) ->
  matcher:start_link(),
  Config.

end_per_testcase(_Case,_Config) ->
  ok.

basic_test(Config) ->
  P = get_test_path(Config,"basic.conf"),
  {ok,Terms} = file:script(P),
  matcher:set_config(Terms),
  {match,[{"127.0.0.1",8080}]} = matcher:match(<<"2342rsfds match_me: someword iweowirf">>),
  ok.


cache_test(Config) ->
  P = get_test_path(Config,"cache.conf"),
  {ok,Terms} = file:script(P),
  matcher:set_config(Terms),
  {match,[{"192.168.1.1",8080}]} = matcher:match(<<"2342rsfds match_me: otherword iweowirf">>),
  nomatch = matcher:match(<<"2342rsfds match_me: otherword1 iweowirf">>),
  ok.


get_test_path(Config,Filename) ->
  DataDir = ?config(data_dir,Config),
  filename:join([DataDir,Filename]).
