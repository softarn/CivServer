-module(testserver1).

-export([init/0, accept/2]).
-define(CHARACTER, :8/unsigned-big-integer).
-define(INTEGER, :32/signed-big-integer).
-define(BOOLEAN, :8/signed-big-integer).
-define(HEADER, :8/signed-big-integer).
-define(PROTOCOLVERSION, 0).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1233, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted Connection\n"),
	spawn(?MODULE, accept, [ListenSocket, Count+1]),
	try
		recv(Socket)
	catch
		error:X ->
			case X of
				{badmatch,{error,closed}} ->
						ok;
					_ ->	 
						io:format("Fångade fel: "), 
						io:format("~p\n", [X]) 
				end
		end.

	readHeader(Socket) -> % Tar ut den första byten och tolkar som Headern utifrån protokollet. 
		{ok, <<Byte?HEADER>>} = gen_tcp:recv(Socket, 1),
		io:format("Read the following header: "),
		io:format("~w\n", [Byte]),
		Byte.

	readInteger(Socket) ->
		{ok, <<Integer?INTEGER>>} = gen_tcp:recv(Socket, 4),
		io:format("Read the following integer: "),
		io:format("~w\n", [Integer]),
		Integer.

	readString(Socket) ->
		readString(Socket, []).
	readString(Socket, List) ->
		{ok, <<Char?CHARACTER>>} = gen_tcp:recv(Socket, 1),
		io:format("~w\n", [Char]),
		case Char of
			0 ->
				io:format("Read the following string: "),
				io:format("~w\n", [lists:reverse(List)]),
				lists:reverse(List);
			_ ->
				readString(Socket, [Char|List])
		end.

	sendString(Socket, List) ->
		gen_tcp:send(Socket, list_to_binary(List)).

	sendInteger(Socket, <<Int?INTEGER>>) ->
		gen_tcp:send(Socket, Int).

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
		FailText = getFailMsg(FailureID), % Text that describes the failure
		FailMsg = list_to_binary([<<0?HEADER>>, FailureID, ReqHeader, FailText]),
		io:format("Created the following failmsg: "),
		io:format("~w\n", [FailMsg]),
		gen_tcp:send(Socket, FailMsg),
		FailMsg.

	recv(Socket) ->
		Header = readHeader(Socket),
		case Header of 
			2 -> % Hello World
				ProtocolVersion = readInteger(Socket),
				io:format("HEJ2"),

				case ProtocolVersion =:= ?PROTOCOLVERSION of
					true ->
						io:format("HEJ!"),
						PlayerName = readString(Socket),
						io:format("~w\n", [PlayerName]),
						Response = list_to_binary([<<3?HEADER>>]),
						gen_tcp:send(Socket, Response);
					false ->
						io:format("NEJ!"),
						createFailPacket(<<0?INTEGER>>, <<Header?HEADER>>, Socket)
				end

		end,
		recv(Socket).
