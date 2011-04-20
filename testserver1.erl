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
    case Char of
        0 ->
            io:format("Read the following string: "),
            io:format("~p\n", [lists:reverse(List)]),
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

sendHeader(Socket, Header) ->
    gen_tcp:send(Socket, <<Header?HEADER>>),
    io:format("SendHeader sent: "),
    io:format("~p\n", [Header]).

sendString(Socket, List) ->
    NewList = List ++ "\0",
    gen_tcp:send(Socket, NewList),
    io:format("SendString sent: "),
    io:format("~p\n", [NewList]).

sendInteger(Socket, Int) ->
    gen_tcp:send(Socket, <<Int?INTEGER>>),
    io:format("SendInteger sent: "),
    io:format("~p\n", [Int]).

sendBoolean(Socket, Bool) ->
    gen_tcp:send(Socket, <<Bool?BOOLEAN>>),
    io:format("SendBoolean sent: "),
    io:format("~p\n", [Bool]).

sendList(Socket, ElemType, List) ->
    NumberOfElements = length(List),
    NewType = ElemType ++ "\0",
    case ElemType of 
        "String" ->
            NewList = [X ++ "\0"|| X <- List], % Lägger till nullterminering på alla strängar i listan
            Packet = [<<NumberOfElements?INTEGER>>, NewType, NewList];
        "Integer" ->
            IntList = [<<X?INTEGER>> || X <- List], % Tolkar alla intar i listan som ?INTEGER
            Packet = [<<NumberOfElements?INTEGER>>, NewType, IntList];
        Other ->
            io:format("Invalid ElemType in function sendList!"),
            Packet = "Error!\0"
    end,	       

    gen_tcp:send(Socket, Packet),
    io:format("SendList sent: "),
    io:format("~p\n", [List]).

sendPerhaps(Socket, false) ->
    sendInteger(Socket, 0).

sendPerhaps(Socket, true, Type, Elem) ->
    sendInteger(Socket, 1),
    sendString(Socket, Type),
    case Type of
        "Integer" ->
            sendInteger(Socket, Elem);
        "String" ->
            sendString(Socket, Elem);
        "Boolean" ->
            sendBoolean(Socket, Elem)
    end.

sendFailPacket(FailureID, ReqHeader, Socket) -> % 1:Number to identify failure. 2:Head of failed package. NEGATIVT FAILUREID FUNGERAR EJ MED LIST2BINARY 
    io:format("Created the following failmsg: "),
    FailText = getFailMsg(FailureID), % Text that describes the failure

    sendHeader(Socket, 0),
    sendInteger(Socket, FailureID),
    sendHeader(Socket, ReqHeader),
    sendString(Socket, FailText).

getFailMsg(FailureType) ->
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
            Msg = "Building does not exist"
    end,
    Msg.


recv(Socket) ->
    Header = readHeader(Socket),
    case Header of 
        1 ->

            sendHeader(Socket, 1),
            sendInteger(Socket, 4),
            sendString(Socket, "hejsan");
        13 ->  
            TheList = readList(Socket),
            io:format("Listan: "),
            io:format("~p\n", [TheList]),
            sendHeader(Socket, 6),
            %sendList(Socket, "Integer", [1, 2, 3, 4, 5, 6, 8]);
            sendList(Socket, "String", ["Maggan", "Fredde", "Simon", "De var ett vackert par"]);
        2 -> % Hello World
            ProtocolVersion = readInteger(Socket),

            case ProtocolVersion =:= ?PROTOCOLVERSION of
                true ->
                    PlayerName = readString(Socket),
                    Response = list_to_binary([<<3?HEADER>>]),
                    gen_tcp:send(Socket, Response);
                false ->
                    readString(Socket),
                    sendFailPacket(0, Header, Socket)
            end
    end,
    recv(Socket).
