-module(metric_handler).

-export([init/2]).

init(Req0,State) ->
  Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/json">>},
        jsone:encode([{metric,metric:get_metric_data()}]),
        Req0),
  {ok, Req, State}.
