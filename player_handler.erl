-module(player_handler).

-compile(export_all). % TA BORT SEN!
-export([]).
-include("config.hrl").

%send som ej ligger i loopen och tar emot en socket, skicka till

init(Socket, FSM_Pid) -> 
    try 
	recv(Socket, FSM_Pid)
    catch
	error:X ->
	    case X of % Socket closed
		{badmatch,{error,closed}} ->
		    ?SERVER:rm_player({socket, Socket}),
		    gen_tcp:close(Socket),
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
	    LoadFlag = ?TCP:readBoolean(Socket),
	    [LoadFlag];

	8 -> % Join request
	    GameName = ?TCP:readString(Socket),
	    [GameName];

	11 -> % Change civilization request
	    NewCiv = ?TCP:readString(Socket),
	    [NewCiv];

	12 ->
	    LockFlag = ?TCP:readBoolean(Socket),
	    [LockFlag];

	13 -> % Start game request
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
	    ?TCP:sendHeader(Socket, Header),
	    ?TCP:sendInteger(Socket, Header);

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
	
	10 -> %Game session information implement later
	    ok;

	14 -> %Start game answer, implement later
	    ok
    end.

%throwPacket(Header, Socket) ->
%    case Header of
%	2 ->
%	    ?TCP:readInteger(Socket),
%	    ?TCP:readString(Socket);
%	4 ->
%	    ?TCP:readBoolean(Socket);
%	5 ->
%	    ok;
%	7 ->
%	    ?TCP:readBoolean(Socket);
%	8 ->
%	    ?TCP:readString(Socket);
%	11 ->
%	    ok;
%	12 ->
%	    ?TCP:readBoolean(Socket);
%	13 ->
%	    ok;
%	15 ->
%	    ?TCP:readList(Socket, "Position");
%	17 ->
%	    ok;
%	19 -> 	ok %To be defined later...
%    end.

send_to_fsm(To, Packet) ->
?P_FSM:send_packet(To, Packet). 
