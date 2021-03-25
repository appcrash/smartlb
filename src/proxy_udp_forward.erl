-module(proxy_udp_forward).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2]).

-include("common.hrl").


start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(State) ->
  R = procket:socket(inet,raw,raw),
  logger:info("@@@@@@@@@@@ ~p",[R]),
  Flags = 16#4000, % don't frag
  Addr = [127,0,0,1],
  Sip = list_to_binary(Addr),
  Dip = list_to_binary(Addr),
  SA = <<
	 2:16/native,
	 00:16/big,
	 Dip/binary,
	 0:64 >>,
  Udp = make_udp_packet(Sip,Dip,3333,4444,<< "Hell0" >>),
  Length = byte_size(Udp) + 20,  % ip header
  IpHdr = make_ip_header(Length,22,Flags,30,17,Sip,Dip),

  {_,Fd} = R,
  Data = << IpHdr/binary, Udp/binary >>,
  procket:sendto(Fd,Data,0,SA),
  {ok,State}.

handle_call(_,_From,State) ->
  logger:info("!!!!!!!!! handle_call"),
  State.

handle_cast(_,State) ->
  logger:info("!!!!!!!!! handle_cast"),
  State.


make_ip_header(Length,Id,Flags,Ttl,Protocol,Sip,Dip) ->
  case can_ip_offload() of
    true ->
      logger:info("offload it ...."),Sum = 0;
    _ ->
      logger:info("can not offload..."),
      Sum = checksum(<<
		       16#45:8, 16#00:8,     %version, ihl, dscp, ecn
		       Length:16/big,
		       Id:16/big,       % identification
		       Flags:16/big,    % Fragment Flags
		       Ttl:8,
		       Protocol:8,
		       0:16,            % checksum before calculation
		       Sip/binary,
		       Dip/binary >>)
  end,
  <<16#45:8,16#00:8,Length:16/big,Id:16/big,Flags:16/big,
    Ttl:8,Protocol:8,Sum:16/big,Sip/binary,Dip/binary >>.



make_udp_packet(Sip,Dip,Sport,Dport,Payload) ->
  Length = byte_size(Payload) + 8, % udp header length(8)
  Udp = <<Sport:16/big, Dport:16/big,Length:16/big,0:16,Payload/binary>>,
  Pseudo_Header = if				% padding payload if needed
		   Length rem 2 =:= 1 -> <<Sip/binary,Dip/binary,17:16/big,Length:16/big,Udp/binary,0:8>>;
		    true -> <<Sip/binary,Dip/binary,17:16/big,Length:16/big,Udp/binary>>
		  end,
  Sum = checksum(Pseudo_Header),
  <<Sport:16/big, Dport:16/big, Length:16/big,
    Sum:16/big,Payload/binary>>.


checksum(Sum) when is_integer(Sum) ->
  if
    Sum=<16#FFFF -> 16#FFFF - Sum;
    true -> checksum((Sum band 16#FFFF) + (Sum bsr 16))
  end;
checksum(Buf) ->
  checksum(lists:foldl(fun(X,Y) -> X+Y end,0,[W || <<W:16>> <= Buf])).

can_ip_offload() ->
  case os:type() of
    {unix,Name} when Name == linux -> % linux raw socket would always fill ip checksum and length if IP_HDRINCL enabled(IPPROTO_RAW implies)
      true;
    _ -> false
  end.
