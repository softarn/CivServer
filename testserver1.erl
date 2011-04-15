-module(testserver1).

-export([init/0, accept/2]).

-define(INTEGER, :32/signed-big-integer).
-define(BOOLEAN, :8/signed-big-integer).
-define(HEADER, :8/signed-big-integer).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted Connection\n"),
	spawn(?MODULE, accept, [ListenSocket, Count+1]),
	recv(Socket).

decodeHeader(<<Head?HEADER, RestOfPacket/binary>>) -> %Tar ut den första byten - signed, TODO: FIXA SIGNED!
	{Head, RestOfPacket}.

decodeInteger(<<Int?INTEGER, RestOfPacket/binary>>) -> %Tar ut de första 4 bytes och tolkar som Integer. - FIXA SIGNED!
	{Int, RestOfPacket}.

fail(FailureType) ->
	case FailureType of

		-1 ->
			Msg = "Invalid state";
		0 ->
			Msg = "Invalid protocol version";
		1 ->
			Msg = "Name already exists";
		2 ->
			Msg = "Game does not exist";
		3 ->
			Msg = "Game is locked";
		4 ->
			Msg = "Permission denied";
		5 ->
			Msg = "Occupied tile";
		6 ->
			Msg = "No movement left";
		7 ->
			Msg = "Invalid tile";
		8 ->
			Msg = "Build in process";
		9 ->
			Msg = "Building already exists";
		10 ->
			Msg = "Friendly fire is off";
		11 ->
			Msg = "Out of range";
		12 ->
			Msg = "Building does not exist";
end,
Msg.

recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} -> 
			{Header, RestOfMsg} = decodeHeader(Packet),
			case Header of 
				2 ->
					{Int, RestOfMsg2} = decodeInteger(RestOfMsg),
					case Int =:= -1 of
						true ->
							io:format("-1"),
							Msg = list_to_binary([3]),
							gen_tcp:send(Socket, Msg);
						false ->
							B1 = <<0:8>>,
							B2 = <<0:32>>,
							FailMsg = list_to_binary([B1, B2, fail(0)]), 
							io:format("~w\n", [FailMsg]),
							gen_tcp:send(Socket, FailMsg)
					end
			end,
			recv(Socket);

		{error,closed} ->
			io:format("Closed connection\n")
	end.
