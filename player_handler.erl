-module(player_handler).

-compile(export_all). % TA BORT SEN!
-export([]).
-include("config.hrl").

init(Player, Parent) -> 
    nope.

%SOCKET CLOSE? vAD HÄNDER?
recv_player(Socket, Parent) ->
    Header = ?TCP:readHeader(Socket),
    case Header of
	2 -> % Hello World. Glöm ej att göra något med PlayerName!!!!!!
	    ProtocolVersion = ?TCP:readInteger(Socket),

	    case ProtocolVersion =:= ?P_VERSION of
		true ->
		    PlayerName = ?TCP:readString(Socket),
		    Player = #player{name = PlayerName, %Create player
				    ref = make_ref(),
			    	    socket = Socket,
			    	    handler= self()},
		    Parent:add_player(Player),
		    ?TCP:sendHeader(Socket, 3), % Welcome to the Real world
		    recv_lobby(Player, Parent);
		false ->
		    ?TCP:readString(Socket),
		    ?TCP:sendFailPacket(Socket, 0, Header) %Send fail packet, wrong protocolversion
	    end;
	_ -> 
	    ?TCP:sendFailPacket(Socket, -1, Header) % failpacket - "Invalid state"
    end.

recv_lobby(Player, Parent) ->
    Socket = Player#player.socket,
    Header = ?TCP:readHeader(Socket),
    case Header of 
	5 ->  % List game request
	    
	    ListOfGames = [];% Hämta available games... och skicka tillbaka som List<String>
	7 -> % Host request
		case ?TCP:readBoolean(Socket) of
		0 -> %false
		    'creategame()', % gör nytt spel
		    GameName = "Hämta gamename",
		    ?TCP:sendHeader(Socket, 9), % Join answer
		    ?TCP:sendString(Socket, GameName);
		_ ->
		    'loadgame()' % ladda spel

	    end;
	8 -> % Join request
	    HostName = ?TCP:readString(Socket),
	    % Kolla upp om spelet finns ELLER om det är låst - isf fail'd
	    % Annars 9 - joinanswer
	    GameName = "Hämta gamename",
	    ?TCP:sendHeader(Socket, 9), % Join answer
	    ?TCP:sendString(Socket, GameName)
%	11 ->
%	    GameLocked = "Hämta info om gameIsLocked()",
%	    case gameLocked of
%		true ->
%		    ?TCP:sendFailPacket(Socket, 11, Header); % Fail'd
%		false ->
%		    ?TCP:sendGameSessionInformation
%	    end
%
	    %TheList = readList(Socket, "String"),
	    %io:format("Listan: "),
	    %io:format("~p\n", [TheList]),
	    %sendHeader(Socket, 6),
	    %sendList(Socket, "Integer", [1, 2, 3, 4, 5, 6, 8]);
	    %sendList(Socket, "String", ["Maggan", "Fredde", "Simon", "De var ett vackert par"]);
    end,
    recv_lobby(Player, Parent).

%recv_gamelobby(Socket) ->
%	.
%recv_game(Socket) ->
%	.
