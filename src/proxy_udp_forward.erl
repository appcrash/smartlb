-module(proxy_udp_forward).
-behaviour(gen_server).

-export([start_link/0,init/1,handle_call/3,handle_cast/2,terminate/2]).
-export([forward/1]).

-include("common.hrl").

-define(UDP_PROTOCOL,17).
-define(IP_TTL,60).
-define(UDP_MTU,1480). % 1500(ethernet) - 20(ip_hdr)


%% this forwarder use raw socket so permission should be promoted
%% in linux, use following command:
%% setcap cap_net_raw=ep `which beam.smp`
%% to ensure erlang process can create raw socket;
%% in freebsd, root is required

-record(server_state,{
  rawsocket,		% used to forward udp packet
  ip_ident		% increasing identification in ip header
}).

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

init(_) ->
  case prepare_raw_socket() of
    {ok,Socket} ->
      State = #server_state{rawsocket = Socket,ip_ident = 1},
      {ok,State};
    {error,Reason} ->
      logger:error("udp proxy forwarder create raw socket error ~p",[Reason]),
      {stop,Reason}
  end.

handle_call(Req,_From,State) ->
  {reply,Req,State}.

handle_cast({forward,{Sip,Sport,Payload}},
	    State=#server_state{rawsocket = Socket,ip_ident = Ident}) ->
  case trait:analyze(Payload) of
    {match,{DipStr,Dport}} ->
      {_,Dip} = inet:parse_ipv4_address(DipStr),
      send_udp_packet(Socket,Ident,Sip,Dip,Sport,Dport,Payload),
      metric:event(udp_matched),
      {noreply,State#server_state{ip_ident = Ident + 1}};
    no_match ->
      metric:event(udp_no_match),
      {noreply,State}
  end;
handle_cast(stop,State) ->
  {stop,normal,State}.

terminate(Reason, _State = #server_state{rawsocket = Socket}) ->
  logger:error("udp proxy forwarder terminated ~p",[Reason]),
  socket:close(Socket).

% forward Data collected by gen_udp
forward(Data) ->
  gen_server:cast(?MODULE,{forward,Data}).

prepare_raw_socket() ->
  case os:type() of
      {unix,linux} ->
        socket:open(inet,raw,{raw,255});         % IPPROTO_RAW: 255
      {unix,freebsd} ->
        case socket:open(inet,raw,{raw,17}) of   % IPPROTO_UDP: 17
	  {ok,Socket} ->
            socket:setopt(Socket,0,2,<<1:32>>),  % IPPROTO_IP:0, IP_HDRINCL:2
	    {ok,Socket};
	  R -> R
	end
  end.


-spec send_udp_packet(integer(),integer(),inet:ip4_address(),inet:ip4_address(),integer(),integer(),binary()) -> atom().
send_udp_packet(RawSocket,Ident,Sip,Dip,Sport,Dport,Payload) ->
  SipBin = list_to_binary(tuple_to_list(Sip)),
  DipBin = list_to_binary(tuple_to_list(Dip)),
  UdpHdrAndPayload = <<0:64,Payload/binary>>, % prepend fake udp header(8 bytes) just to calc fragments
  TotalUdpLength = byte_size(UdpHdrAndPayload),
  lists:foreach(fun({Flag,Offset,P}) ->
	      UdpPacket = make_udp_packet(SipBin,DipBin,Sport,Dport,Ident,Flag,Offset,TotalUdpLength,P),
	      %logger:info("***** ~p  ~p ",[Flag,Offset]),
	      socket:sendto(RawSocket,UdpPacket,
			       #{family => inet,
				addr => Dip,
				port => Dport})
	  end,fragment(UdpHdrAndPayload)).

fragment(Payload) -> fragment(Payload,0,[]).
fragment(Payload,Offset,L) when byte_size(Payload) =< ?UDP_MTU ->
  % final packet of the fragments, reverse it so that first comes first
  if
    Offset == 0 -> lists:reverse([{dont_frag,0,Payload} | L]); % no fragment at all
    true -> lists:reverse([{last_frag,Offset,Payload} | L]) % last fragment
  end;
fragment(Payload,Offset,L) ->
  IncreasedOffset = ?UDP_MTU div 8,  % offset in unit of 8-bytes
  Fragmented_Bytes = IncreasedOffset bsl 3,  % multiply 8 get consumed bytes
  NewOffset = Offset + IncreasedOffset,
  <<P:Fragmented_Bytes/binary-unit:8,Remain/binary >> = Payload,
  %logger:info("~p --------------- ~p",[byte_size(FragPayload),byte_size(Remain)]),
  fragment(Remain,NewOffset,[{more_frag,Offset,P} | L]).

udp_action(dont_frag,_) -> {calc_pseudo_header,<<16#4000:16>>};
udp_action(more_frag,Offset) when Offset == 0 -> {calc_pseudo_header,<<16#2000:16>>}; % first fragment need udp header
udp_action(more_frag,Offset) -> {only_payload,<<2#001:3,Offset:13/big>>};
udp_action(last_frag,Offset) -> {only_payload,<<0:3,Offset:13/big>>}.

-spec make_udp_packet(binary(),binary(),integer(),integer(),integer(),atom(),integer(),integer(),binary()) -> binary().
make_udp_packet(Sip,Dip,Sport,Dport,Ident,Flag,Offset,TotalUdpLength,Payload) ->
  PL = byte_size(Payload),
  IpLength = byte_size(Payload) + 20, % ip header(20)
  case udp_action(Flag,Offset) of
    {calc_pseudo_header,FlagBin} -> % ip payload is udp header + udp payload(all or partial)
      <<_:8/binary,RealPayload/binary>> = Payload,  % strip the fake udp header(8 bytes)
      Udp = <<Sport:16/big, Dport:16/big,TotalUdpLength:16/big,0:16,RealPayload/binary>>,
      Pseudo_Header =
	if				% padding payload if needed
	  PL rem 2 =:= 1 -> <<Sip/binary,Dip/binary,17:16/big,PL:16/big,Udp/binary,0:8>>;
	  true -> <<Sip/binary,Dip/binary,17:16/big,PL:16/big,Udp/binary>>
	end,
						% calculate checksum with pseudo header or set it as 0 directly without any
						% calculation, which means checksum isn't being used
      Sum = checksum(Pseudo_Header),
      IpHeader = make_ip_header(IpLength,Ident,FlagBin,?UDP_PROTOCOL,Sip,Dip),
      %logger:info("calc pseudo header offset:~p ~p",[Offset,utils:bit_format(IpHeader)]),
      <<IpHeader/binary, Sport:16/big, Dport:16/big, TotalUdpLength:16/big,
	Sum:16/big,RealPayload/binary>>;
    {only_payload,FlagBin} -> % ip payload is data(no udp header) when 'more frag' is set or the last frag
      IpHeader = make_ip_header(IpLength,Ident,FlagBin,?UDP_PROTOCOL,Sip,Dip),
      <<IpHeader/binary,Payload/binary>>
  end.

make_ip_header(Length,Id,Flags,Protocol,Sip,Dip) ->
  case can_ip_offload() of
    true -> Sum = 0;
    _ -> Sum = checksum(<<
		       16#45:8, 16#00:8, % version, ihl, dscp, ecn        2-bytes
		       Length:16/big,    % length                         2-bytes
		       Id:16/big,        % identification                 2-bytes
		       Flags/binary,     % fragment flags                 2-bytes
		       ?IP_TTL:8,        % time to live                   1-byte
		       Protocol:8,       % upper protocol                 1-byte
		       0:16,             % checksum before calculation    2-bytes
		       Sip/binary,       % source ip                      2-bytes
		       Dip/binary >>)    % destination ip                 2-bytes
  end,
  <<16#45:8,16#00:8,Length:16/big,Id:16/big,Flags/binary,
    ?IP_TTL:8,Protocol:8,Sum:16/big,Sip/binary,Dip/binary>>.

checksum(Sum) when is_integer(Sum) ->
  if
    Sum =< 16#FFFF -> 16#FFFF - Sum;
    true -> checksum((Sum band 16#FFFF) + (Sum bsr 16))
  end;
checksum(Buf) ->  % Buf should be bitstring aligned to 16-bit, i.e. padding if needed
  checksum(lists:foldl(fun(X,Y) -> X+Y end,0,[W || <<W:16>> <= Buf])).

can_ip_offload() ->
  case os:type() of
    {unix,_}  -> % linux raw socket would always fill ip checksum and length if IP_HDRINCL enabled(IPPROTO_RAW implies)
      true;
    _ -> false
  end.
