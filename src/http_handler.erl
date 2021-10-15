-module(http_handler).

-export([init/2]).

init(Req,State) ->
  #{path := Path} = Req,
  logger:info("http request path ~p",[Path]),
  Res = handle(Path,Req),
  {ok, Res, State}.


handle(<<"/metric">>,Req) ->
  cowboy_req:reply(200,
		   #{<<"content-type">> => <<"text/json">>},
		   jsone:encode([{metric,metric:get_metric_data()}]),
		   Req);
handle(<<"/update">>,Req) ->
  case lb:reload_config() of
    {ok,Terms} ->
      logger:info("update config ok"),
      Str = io_lib:format("~p",[Terms]),
      cowboy_req:reply(200,
		       #{<<"content-type">> => <<"text/json">>},
		       jsone:encode([{ok,list_to_binary(Str)}]),
		       Req);
    {error,Desc} ->
      logger:info("update config failed"),
      cowboy_req:reply(200,
		       #{<<"content-type">> => <<"text/json">>},
		       jsone:encode([{error,list_to_binary(Desc)}]),
		       Req)
  end;
handle(_,Req) ->
  cowboy_req:reply(404,Req).
