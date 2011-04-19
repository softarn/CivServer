-module(testserver1).

-export([init/0, accept/2]).

-define(CHARACTER, 	:8/unsigned-big-integer).
-define(INTEGER, 	:32/signed-big-integer).
-define(BOOLEAN, 	:8/signed-big-integer).
-define(HEADER, 	:8/signed-big-integer).
-define(PROTOCOLVERSION, 0).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
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

readBoolean(Socket) -> % Tar ut den första byten och tolkar som boolean utifrån protokollet. 
	{ok, <<Byte?BOOLEAN>>} = gen_tcp:recv(Socket, 1),
	io:format("Read the following boolean: "),
	io:format("~w\n", [Byte]),
	Byte.

readInteger(Socket) ->% Tar ut de första 4 bytes och tolkar som Integer utifrån protokollet. 
	{ok, <<Integer?INTEGER>>} = gen_tcp:recv(Socket, 4),
	io:format("Read the following integer: "),
	io:format("~w\n", [Integer]),
	Integer.

readString(Socket) ->% Tar ut bytes tills /0 påträffas, tolkar sedan som String utifrån protokollet
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

readList(Socket) ->
	NumberOfElements = readInteger(Socket),
	ElemType = readString(Socket),
	readList(Socket, NumberOfElements, ElemType, []).


readList(_, 0, _, List) ->
	lists:reverse(List);

readList(Socket, NumberOfElements, "Integer", List) ->
	Element = readInteger(Socket),
	readList(Socket, NumberOfElements-1, "Integer", [Element|List]);

readList(Socket, NumberOfElements, "Boolean", List) ->
	Element = readBoolean(Socket),
	readList(Socket, NumberOfElements-1, "Boolean", [Element|List]);

readList(Socket, NumberOfElements, "String", List) ->
	Element = readString(Socket),
	readList(Socket, NumberOfElements-1, "String", [Element|List]).

readPerhaps(Socket) ->
	Perhaps = readBoolean(Socket),
	case Perhaps of
		1 ->
			ElemType = readString(Socket),
			readPerhaps(Socket, ElemType);
		_ ->
			none
	end.

readPerhaps(Socket, "Integer") ->
	readInteger(Socket);

readPerhaps(Socket, "String") ->
	readString(Socket);

readPerhaps(Socket, "Boolean") ->
	readBoolean(Socket).

sendString(Socket, List) ->
	gen_tcp:send(Socket,list_to_binary(List)).

sendInteger(Socket, Int) ->
	gen_tcp:send(Socket, Int).

sendBoolean(Socket, <<Bool?BOOLEAN>>) ->
	gen_tcp:send(Socket, Bool).

sendList(Socket, List) ->
	gen_tcp:send(Socket, list_to_binary(List)).

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
		 13 ->  
			%io:format("Skickat");
			TheList = readList(Socket),
			io:format("Listan: "),
			io:format("~p\n", [TheList]),
			createFailPacket(<<10?INTEGER>>, <<Header?HEADER>>, Socket);
		2 -> % Hello World
			ProtocolVersion = readInteger(Socket),

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
