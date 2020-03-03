-module(utils).

-export([get_config/2]).

get_config(Key,DefaultValue) ->
    case application:get_env(Key) of
      {ok,P} ->
	logger:info("key(~p)'s default value is ~p, now is ~p~n",[Key,DefaultValue,P]),
	P;
      _ -> DefaultValue
    end.
