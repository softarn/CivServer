-module(player_handler).

-compile(export_all). % TA BORT SEN!
-export([]).
-include("config.hrl").

init(Socket, FSM_Pid) -> 
    try 
	recv(Socket, FSM_Pid)
    catch
	error:X ->
	    case X of % Socket closed
		{badmatch,{error,closed}} ->
		    ?SERVER:rm_player({socket, Socket}),
		    ok; %Remove playaah
		_ ->
		    io:format("Caught error in player_handler: ~p\n", [X])
	    end
    end.

recv(Socket, FSM) ->
    Header = ?TCP:readHeader(Socket),

    RestOfMsg = case Header of

	2 -> % Hello World
	    PV = ?TCP:readInteger(Socket),
	    PN = ?TCP:readString(Socket),
	    [PV,PN];

	5 ->  % List game request
	    [];

	7 -> % Host request
	    [];

	8 -> % Join request
	    GameName = ?TCP:readString(Socket),
	    [GameName];

	11 -> % Change civilization request
	    NewCiv = ?TCP:readString(Socket),
	    [NewCiv];

	12 -> % Lock game request
	    LockFlag = ?TCP:readBoolean(Socket),
	    [LockFlag];

	13 -> % Start game request
	    [];

	15 -> %Move request
	    MovePath = ?TCP:readList(Socket, "Position"),
	    [MovePath];

	16 -> % End of turn
	    [];

	23 -> %Spawned
	    Pos = ?TCP:readElement(Socket, "Position"),
	    Unit = ?TCP:readElement(Socket, "Unit"),
	    [Pos, Unit];

	24 -> %Exit game
	    []
    end,

    Packet = {Header, RestOfMsg},
    send_to_fsm(FSM, Packet),
    recv(Socket, FSM).

sendFailMsg(Socket, ID, Header) ->
    ?TCP:sendFailPacket(Socket, ID, Header).

sendMsg(Socket, {Header, List}) ->
    case Header of

	1 -> %Confirm'd
	    [Head] = List,
	    ?TCP:sendHeader(Socket, Header),
	    ?TCP:sendHeader(Socket, Head);

	3 -> %Welcome to the Real world
	    ?TCP:sendHeader(Socket, Header);

	4 -> %Ping, implement later
	    ok;

	6 -> %List game answer
	    [Games] = List,
	    ?TCP:sendHeader(Socket, Header),
	    ?TCP:sendList(Socket, "String", Games);

	9 -> %Join answer
	    [HostName] = List,
	    ?TCP:sendHeader(Socket, Header),
	    ?TCP:sendString(Socket, HostName);

	10 -> %Game session information
	    [PList, Locked] = List,
	    ?TCP:sendHeader(Socket, 10), %Game session information
	    ?TCP:sendList(Socket, "Player", PList),
	    ?TCP:sendBoolean(Socket, Locked);				

	14 -> %Start game answer, implement later
	    [Map, TL] = List,
	    TileList = lists:flatten(?GAMEPLAN:tuplemap_to_listmap(TL)), %Gör om tuplemappen till en lista och skicka
	    ?TCP:sendHeader(Socket, Header),
	    ?TCP:sendList(Socket, "Column", Map),
	    ?TCP:sendList(Socket, "Tile", TileList);

	17 -> % It's your turn
	    [TL] = List,
	    TileList = lists:flatten(?GAMEPLAN:tuplemap_to_listmap(TL)), %Gör om tuplemappen till en lista och skicka
	    ?TCP:sendHeader(Socket, 17), %It's your turn
	    ?TCP:sendList(Socket, "Tile", TileList);
	
	25 -> % Game Closed
	    ?TCP:sendHeader(Socket, 25)
    end.

send_to_fsm(To, Packet) ->
    ?P_FSM:send_packet(To, Packet). 
