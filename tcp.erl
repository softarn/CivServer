-module(tcp).

-export([init/2]).
-export([accept/2, recv_pname/1]).

-define(CHARACTER, 	:8/unsigned-big-integer).
-define(INTEGER, 	:32/signed-big-integer).
-define(BOOLEAN, 	:8/signed-big-integer).
-define(HEADER, 	:8/signed-big-integer).
-define(PROTOCOLVERSION, 0).

init(Parent, Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept(ListenSocket, Parent).

accept(ListenSocket, Parent) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Accepted Connection\n"),
    Parent ! Socket,
    accept(ListenSocket, Parent).

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

readList(Socket, ElemType) ->
    NumberOfElements = readInteger(Socket),
    readList(Socket, NumberOfElements, ElemType, []).

readList(_, 0, _, List) ->
    lists:reverse(List);

readList(Socket, NumberOfElements, ElemType, List) ->
    Element = readElement(Socket, ElemType),
    readList(Socket, NumberOfElements-1, ElemType, [Element|List]).

readPerhaps(Socket, ElemType) ->
    Perhaps = readBoolean(Socket),
    case Perhaps of
	0 ->
	    none;
	_ ->
	    readElement(Socket, ElemType)
    end.

readElement(Socket, "Integer") ->
    readInteger(Socket);

readElement(Socket, "String") ->
    readString(Socket);

readElement(Socket, "Boolean") ->
    readBoolean(Socket);

readElement(Socket, "List") ->
    readList(Socket, "List");

readElement(Socket, "Unit") ->
    Owner = readString(Socket),
    UnitType = readString(Socket),
    ManPower = readInteger(Socket),
    {unit, Owner, UnitType, ManPower};

readElement(Socket, "City") ->
    Owner = readString(Socket),
    UnitList = readList(Socket, "String"),
    Buildings = readList(Socket, "String"),
    Name = readString(Socket),
    {city, Owner, UnitList, Buildings, Name};

readElement(Socket, "Player") -> % Lista av Player
    Name = readString(Socket),
    Civ = readString(Socket),
    {player, Name, Civ};

readElement(Socket, "Position") -> % Lista av Position
    X = readInteger(Socket),
    Y = readInteger(Socket),
    {position, X, Y};

readElement(Socket, "Column") -> % Lista av Column
    Column = readList(Socket, "String"),  
    {column, Column}.


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

    case ElemType of 
	"String" ->
	    NewList = [X ++ "\0"|| X <- List], % Lägger till nullterminering på alla strängar i listan
	    Packet = [<<NumberOfElements?INTEGER>>, NewList],
	    gen_tcp:send(Socket, Packet);
	"Player" ->
	    NewList = [X ++ "\0"|| X <- List], % Lägger till nullterminering på alla strängar i listan
	    Packet = [<<NumberOfElements?INTEGER>>, NewList],
	    gen_tcp:send(Socket, Packet);
	"Position" ->
	    NewList = [<<X?INTEGER>> || X <- List], % Tolkar alla intar i listan som ?INTEGER
	    Packet = [<<NumberOfElements?INTEGER>>, NewList],
	    gen_tcp:send(Socket, Packet);
	"Integer" ->
	    NewList = [<<X?INTEGER>> || X <- List], % Tolkar alla intar i listan som ?INTEGER
	    Packet = [<<NumberOfElements?INTEGER>>, NewList],
	    gen_tcp:send(Socket, Packet);
	"Column" ->
	    sendInteger(Socket, NumberOfElements), % Skicka antalet listor
	    [sendList(Socket, "String", X) || X <- List]; % Skicka alla listor
	"Unit" ->
	    Fun = fun(X) ->
		    case is_integer(X) of
			true ->
			    sendInteger(Socket, <<X?INTEGER>>);
			false ->
			    sendString(Socket, X ++ "\0")
		    end
	    end,
	    lists:foreach(Fun, List);

	_ ->
	    io:format("Invalid ElemType in function sendList!")
    end,       
    io:format("SendList sent: "),
    io:format("~p\n", [List]).


sendPerhaps(Socket, false) ->
    sendBoolean(Socket, 0).

sendPerhaps(Socket, true, Type, Elem) ->
    sendBoolean(Socket, 1),
    case Type of
	"Integer" ->
	    sendInteger(Socket, Elem);
	"String" ->
	    sendString(Socket, Elem);
	"List" ->
	    sendList(Socket, Type, Elem);
	"Unit" ->
	    {unit, Owner, Type, ManPower} = Elem,
	    sendString(Socket, Owner),
	    sendString(Socket, Type),
	    sendInteger(Socket, ManPower);
	"City" ->
	    {city, Owner, Units, Buildings, Name} = Elem,
	    sendString(Socket, Owner),
	    sendList(Socket, "Unit", Units),
	    sendList(Socket, "String", Buildings),
	    sendString(Socket, Name);
	"Boolean" ->
	    sendBoolean(Socket, Elem)
    end.
%	player = string namn, string civ, 	1 list = player, string, column, position, unit
%	column = list<String>			2 perhaps = unit, city, string, 
%	position = integer x, integer y		3
%	unit = ägare STring, type String, manpower Integer 3
%	city = ägare String, list<Unit>, list<String>, name String 4

sendFailPacket(Socket, FailureID, ReqHeader) -> % 2:Number to identify failure. 3:Head of failed package. NEGATIVT FAILUREID FUNGERAR EJ MED LIST2BINARY 
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

recv_pname(Socket) ->
	io:format("inni i recvpname"),
    Header = readHeader(Socket),
    io:format("läst header"),
    case Header of
	2 -> % Hello World. Glöm ej att göra något med PlayerName!!!!!!
	    ProtocolVersion = readInteger(Socket),

	    case ProtocolVersion =:= ?PROTOCOLVERSION of
		true ->
		    PlayerName = readString(Socket),
		    sendHeader(Socket, 3), % Welcome to the Real world
		    {ok, PlayerName};
		false ->
		    readString(Socket),
		    sendFailPacket(Socket, 0, Header), %Fail'd
		    {error, "Bad protocol version"}
	    end;
	_ -> 
	    sendFailPacket(Socket, -1, Header) % Skicka felmedd "Invalid state"
    end.

send_games(Socket, Games) ->
    sendHeader(Socket, 6), %List game answer
    sendList(Socket, "String", Games).

%socket close?
recv(Socket) ->
    Header = readHeader(Socket),
    case Header of 
	5 ->  % List game request
	    
	    ListOfGames = [];% Hämta available games... och skicka tillbaka som List<String>
	7 -> % Host request
	    case readBoolean(Socket) of
		0 -> %false
		    'creategame()', % gör nytt spel
		    GameName = "Hämta gamename",
		    sendHeader(Socket, 9), % Join answer
		    sendString(Socket, GameName);
		_ ->
		    'loadgame()' % ladda spel

	    end;
	8 -> % Join request
	    HostName = readString(Socket),
	    % Kolla upp om spelet finns ELLER om det är låst - isf fail'd
	    % Annars 9 - joinanswer
	    GameName = "Hämta gamename",
	    sendHeader(Socket, 9), % Join answer
	    sendString(Socket, GameName);
	11 ->
	    GameLocked = "Hämta info om gameIsLocked()",
	    case gameLocked of
		true ->
		    sendFailPacket(Socket, 11, Header); % Fail'd
		false ->
		    sendGameSessionInformation
	    end

	    %TheList = readList(Socket, "String"),
	    %io:format("Listan: "),
	    %io:format("~p\n", [TheList]),
	    %sendHeader(Socket, 6),
	    %sendList(Socket, "Integer", [1, 2, 3, 4, 5, 6, 8]);
	    %sendList(Socket, "String", ["Maggan", "Fredde", "Simon", "De var ett vackert par"]);
    end,
    recv(Socket).
