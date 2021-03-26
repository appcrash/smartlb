-module(proxy_udp_forward).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,terminate/2]).
-export([forward/1]).

-include("common.hrl").

-define(UDP_PROTOCOL,17).
-define(IP_TTL,60).
-define(UDP_PAYLOAD_MTU,1472). % 1500(ethernet) - 20(ip_hdr)


%% this forwarder use raw socket so permission should be promoted
%% in linux, use following command:
%% setcap cap_net_raw=ep `which beam.smp`
%% to ensure erlang process can create raw socket

-record(server_state,{
  rawsocket,		% used to forward udp packet
  ip_ident		% increasing identification in ip header
}).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
  case procket:socket(inet,raw,raw) of
    {ok,SocketFd} ->
      State = #server_state{rawsocket = SocketFd,ip_ident = 1},
      {ok,State};
    {error,Reason} ->
      logger:error("udp proxy forwarder create raw socket error ~p",[Reason]),
      {stop,Reason}
  end.

handle_call(Req,_From,State) ->
  {reply,Req,State}.

handle_cast({forward,{Sip,Sport,Payload}},
	    State=#server_state{rawsocket = Socket,ip_ident = Ident}) ->
  {ok,Dip} = inet:parse_ipv4_address("127.0.0.1"),
  send_udp_packet(Socket,Ident,Sip,Dip,Sport,4444,Payload),
  {noreply,State#server_state{ip_ident = Ident + 1}};
handle_cast(stop,State) ->
  {stop,normal,State}.

terminate(Reason, _State = #server_state{rawsocket = Socket}) ->
  logger:error("udp proxy forwarder terminated ~p",[Reason]),
  procket:close(Socket).

% forward Data collected by gen_udp
forward(Data) ->
  gen_server:cast(?MODULE,{forward,Data}).

-spec send_udp_packet(integer(),integer(),inet:ip4_address(),inet:ip4_address(),integer(),integer(),binary()) -> atom().
send_udp_packet(RawSocket,Ident,Sip,Dip,Sport,Dport,Payload) ->
  SipBin = list_to_binary(tuple_to_list(Sip)),
  DipBin = list_to_binary(tuple_to_list(Dip)),
  % procket sendto need sa_addr as binary, which is not portable :(
  % for linux only
  SA = << 2:16/native,    % sin_family, inet
	  0:16/big,       % sin_port, for raw socket set to zero,
	  DipBin/binary,  % sin_addr
	  0:64 >>,        % padding
  FragFlags = 16#4000,

  UdpLength = byte_size(Payload) + 8, % udp header length(8)
  UdpHdrAndPayload = <<SipBin:16/big,DipBin:16/big,UdpLength:16/big,0:16,Payload/binary>>,
  Fragmented_Payload = fragment(UdpHdrAndPayload,0),
  UdpPacket = make_udp_packet(SipBin,DipBin,Sport,Dport,Ident,FragFlags,Payload),
  procket:sendto(RawSocket,UdpPacket,0,SA).

fragment(Payload,Offset) -> fragment(Payload,Offset,[]).
fragment(Payload,Offset,L) when byte_size(Payload) =< ?UDP_PAYLOAD_MTU ->
  % final packet of the fragments, reverse it to keep first comes first
  if
    Offset == 0 -> lists:reverse([{make_ip_frag(dont_frag,0),Payload} | L]);
    true -> lists:reverse([{make_ip_frag(none,offset),Payload} | L])
  end;
fragment(Payload,Offset,L) ->
  IncreasedOffset = ?UDP_PAYLOAD_MTU div 8,  % offset in unit of 8-bytes
  Fragmented_Bytes = IncreasedOffset bsl 3,  % multiply 8 get consumed bytes
  NewOffset = Offset + IncreasedOffset,
  <<FragPayload:Fragmented_Bytes/binary-unit:8,Remain/binary >> = Payload,
  fragment(Remain,NewOffset,[{make_ip_frag(more_frag,Offset),FragPayload},L]).


-spec make_ip_frag(atom(),integer()) -> binary().
make_ip_frag(flag,offset) ->
  case flag of
    dont_frag -> << 2#100:3, offset:13/big >>;
    more_frag -> << 2#010:3, offset:13/big >>;
    _ -> << 0:3, offset:13/big >>
  end.

-spec make_udp_packet(binary(),binary(),integer(),integer(),integer(),integer(),binary()) -> binary().
make_udp_packet(Sip,Dip,Sport,Dport,Ident,FragFlags,Payload) ->
  UdpLength = byte_size(Payload) + 8, % udp header length(8)
  IpLength = UdpLength + 20,	      % ip header(20)
  Udp = <<Sport:16/big, Dport:16/big,UdpLength:16/big,0:16,Payload/binary>>,
  Pseudo_Header =
    if				% padding payload if needed
      UdpLength rem 2 =:= 1 -> <<Sip/binary,Dip/binary,17:16/big,UdpLength:16/big,Udp/binary,0:8>>;
      true -> <<Sip/binary,Dip/binary,17:16/big,UdpLength:16/big,Udp/binary>>
    end,
  % calculate checksum with pseudo header or set it as 0 directly without any
  % calculation, which means checksum isn't being used
  Sum = checksum(Pseudo_Header),
  IpHeader = make_ip_header(IpLength,Ident,FragFlags,?UDP_PROTOCOL,Sip,Dip),
  <<IpHeader/binary, Sport:16/big, Dport:16/big, UdpLength:16/big,
    Sum:16/big,Payload/binary>>.


make_ip_header(Length,Id,Flags,Protocol,Sip,Dip) ->
  case can_ip_offload() of
    true -> Sum = 0;
    _ -> Sum = checksum(<<
		       16#45:8, 16#00:8,     %version, ihl, dscp, ecn
		       Length:16/big,
		       Id:16/big,       % identification
		       Flags:16/big,    % Fragment Flags
		       ?IP_TTL:8,
		       Protocol:8,
		       0:16,            % checksum before calculation
		       Sip/binary,
		       Dip/binary >>)
  end,
  <<16#45:8,16#00:8,Length:16/big,Id:16/big,Flags:16/big,
    ?IP_TTL:8,Protocol:8,Sum:16/big,Sip/binary,Dip/binary >>.

checksum(Sum) when is_integer(Sum) ->
  if
    Sum=<16#FFFF -> 16#FFFF - Sum;
    true -> checksum((Sum band 16#FFFF) + (Sum bsr 16))
  end;
checksum(Buf) ->  % Buf should be bitstring aligned to 16-bit, i.e. padding if needed
  checksum(lists:foldl(fun(X,Y) -> X+Y end,0,[W || <<W:16>> <= Buf])).

can_ip_offload() ->
  case os:type() of
    {unix,Name} when Name == linux -> % linux raw socket would always fill ip checksum and length if IP_HDRINCL enabled(IPPROTO_RAW implies)
      true;
    _ -> false
  end.
