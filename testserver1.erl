-module(testserver1).

-export([init/0, accept/2]).

-define(INTEGER, :32/signed-big-integer).
-define(BOOLEAN, :8/signed-big-integer).
-define(HEADER, :8/signed-big-integer).
-define(PROTOCOLVERSION, 0).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted Connection\n"),
	spawn(?MODULE, accept, [ListenSocket, Count+1]),
	recv(Socket).

decodeHeader(<<Head?HEADER, RestOfPacket/binary>>) -> % Tar ut den första byten och tolkar som Headern utifrån protokollet. SKA VARA BINARY ELLER BITS/BITSTRING? BINARY TROR JAG MÅSTE VARA DELBART MED 8
	{Head, RestOfPacket}.

decodeInteger(<<Int?INTEGER, RestOfPacket/binary>>) -> % Tar ut de första 4 bytes och tolkar som Integer utifrån protokollet.
	{Int, RestOfPacket}.

getFailMsg(FailureType) ->
	case FailureType of
		-1 ->
			Msg = "Invalid state\0";
		0 ->
			Msg = "Invalid protocol version\0";
		1 ->
			Msg = "Name already exists\0";
		2 ->
			Msg = "Game does not exist\0";
		3 ->
			Msg = "Game is locked\0";
		4 ->
			Msg = "Permission denied\0";
		5 ->
			Msg = "Occupied tile\0";
		6 ->
			Msg = "No movement left\0";
		7 ->
			Msg = "Invalid tile\0";
		8 ->
			Msg = "Build in process\0";
		9 ->
			Msg = "Building already exists\0";
		10 ->
			Msg = "Friendly fire is off\0";
		11 ->
			Msg = "Out of range\0";
		12 ->
			Msg = "Building does not exist\0"
end,
Msg.

createFailPacket(<<FailureID?INTEGER>>, <<ReqHeader?HEADER>>, Socket) -> % 1:Number to identify failure. 2:Head of failed package. NEGATIVT FAILUREID FUNGERAR EJ MED LIST2BINARY 
	io:format("Inne i createFailPacket\n"),
	FailText = getFailMsg(FailureID), % Text that describes the failure
	io:format(FailText),
	FailMsg = list_to_binary([<<0?HEADER>>, FailureID, ReqHeader, FailText]),
	%io:format("~w\n", [FailMsg]),
	gen_tcp:send(Socket, FailMsg),
	FailMsg.

recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} -> 
			{Header, PacketAfterHeader} = decodeHeader(Packet),
			case Header of 
				2 ->
					{Int, RestOfMsg2} = decodeInteger(PacketAfterHeader),
					io:format("Int = "), 
					io:format("~w\n", [Int]),
					case Int =:= ?PROTOCOLVERSION of
						true ->
							Msg = list_to_binary([3]),
							gen_tcp:send(Socket, Msg);
						false ->
							createFailPacket(<<0?INTEGER>>, <<Header?HEADER>>, Socket)

						createFailPacket(<<0?INTEGER>>, <<Header?HEADER>>, Socket)
					end
			end,
			recv(Socket);

		{error,closed} ->
			io:format("Closed connection\n")
	end.
