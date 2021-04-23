-module(utils).

-export([get_config/2,index_of/2]).
-export([bit_format/1]).

get_config(Key,DefaultValue) ->
    case application:get_env(Key) of
      {ok,P} ->
	logger:info("key(~p)'s default value is ~p, now is ~p",[Key,DefaultValue,P]),
	P;
      _ -> DefaultValue
    end.

%% find the postion of element in list, none if not found
%% NOTE: the returned index assumes list is 0-based
-spec index_of(any(),list()) -> none | integer().
index_of(Element,List) -> index_of(Element,List,0).
index_of(_,[],_) -> none;
index_of(Element,[H|T],N) ->
  if
    Element == H -> N;
    true -> index_of(Element,T,N+1)
  end.


% convert bitstring to list of hex number string for debugging
-spec bit_format(bitstring()) -> list().
bit_format(B) ->
  [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= B ]].
