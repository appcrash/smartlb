-module(utils).

-export([get_config/2]).
-export([bit_format/1]).

get_config(Key,DefaultValue) ->
    case application:get_env(Key) of
      {ok,P} ->
	logger:info("key(~p)'s default value is ~p, now is ~p",[Key,DefaultValue,P]),
	P;
      _ -> DefaultValue
    end.

% convert bitstring to list of hex number string for debugging
-spec bit_format(bitstring()) -> list().
bit_format(B) ->
  [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= B ]].
