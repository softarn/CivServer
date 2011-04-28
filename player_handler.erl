-module(player_handler).

-compile(export_all). % TA BORT SEN!
-export([]).
-include("config.hrl").

init(Socket, Parent) -> 
	try 
		recv_player(Socket, Parent)
	catch
		error:X ->
			case X of % Socket closed
				{badmatch,{error,closed}} ->
					ok;%Remove playaah
				_ ->
					io:format("Fångade fel från player_handler~p\n", [X])
			end
	end.

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
		?TCP:sendFailPacket(Socket, -1, Header), % failpacket - "Invalid state"
	    	throwPacket(Header, Socket)
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
			?SERVER:create_game(Player),
			?TCP:sendHeader(Socket, 9), % Join answer
		    	?TCP:sendString(Socket, Player#player.name);
		_ ->
		    'loadgame()' % ladda spel

	    end;
	8 -> % Join request
	    HostName = ?TCP:readString(Socket),
	    % Kolla upp om spelet finns ELLER om det är låst - isf fail'd
	    % Annars 9 - joinanswer
	    GameName = "Hämta gamename",
	    ?TCP:sendHeader(Socket, 9), % Join answer
	    ?TCP:sendString(Socket, GameName);
    
    _Other ->
	?TCP:sendFailPacket(Socket, -1, Header) %Fail packet invalid state
    
    end,
    recv_lobby(Player, Parent).

%recv_gamelobby(Socket) ->
%	.
%recv_game(Socket) ->
%	.

throwPacket(Header, Socket) ->
	case Header of
		2 ->
			?TCP:readInteger(Socket),
			?TCP:readString(Socket);
		4 ->
			?TCP:readBoolean(Socket);
		5 ->
			ok;
		7 ->
			?TCP:readBoolean(Socket);
		8 ->
			?TCP:readString(Socket);
		11 ->
			ok;
		12 ->
			?TCP:readBoolean(Socket);
		13 ->
			ok;
		15 ->
			?TCP:readList(Socket, "Position");
		17 ->
			ok;
		19 -> 	ok %To be defined later...
	end.
