-module(matcher_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
  [basic_test,cache_test].



init_per_testcase(_Case,Config) ->
  matcher_master:start_link(),
  Config.

end_per_testcase(_Case,_Config) ->
  ok.

basic_test(Config) ->
  set_matcher_config(Config,"basic.conf"),
  {match,[{"127.0.0.1",8080}]} = match_by(<<"2342rsfds match_me: someword iweowirf">>),
  ok.


cache_test(Config) ->
  set_matcher_config(Config,"cache.conf"),
  {match,[{"192.168.1.1",8080}]} = match_by(<<"2342rsfds match_me: otherword iweowirf">>),
  nomatch = match_by(<<"2342rsfds match_me: otherword1 iweowirf">>),
  ok.

match_by(Data) ->
  matcher_master:match(Data).

set_matcher_config(Config,Filename) ->
  DataDir = ?config(data_dir,Config),
  P = filename:join([DataDir,Filename]),
  {ok,Terms} = file:script(P),
  FlowFuncs = matcher_builder:compile(Terms),
  matcher_master:set_config(FlowFuncs).
