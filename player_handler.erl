-module(player_handler).

-compile(export_all). % TA BORT SEN!
-export([]).
-include("config.hrl").

init(Socket) -> 
    try 
	recv_player(Socket)
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

recv_player(Socket) ->
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
			handler = self()},

		    case ?SERVER:add_player(Player) of
			true ->
			    ?TCP:sendHeader(Socket, 3), % Welcome to the Real world
			    recv_lobby(Player);
			false ->
			    ?TCP:sendFailPacket(Socket, 1, Header), %Fail packet name already exists
			    recv_player(Socket)
		    end;

		false ->
		    ?TCP:readString(Socket),
		    ?TCP:sendFailPacket(Socket, 0, Header) %Send fail packet, wrong protocolversion
	    end;

	_ -> 
	    ?TCP:sendFailPacket(Socket, -1, Header), % failpacket - "Invalid state"
	    throwPacket(Header, Socket),
	    recv_player(Socket)
    end.

recv_lobby(Player) ->
    Socket = Player#player.socket,
    Header = ?TCP:readHeader(Socket),
    case Header of 
	5 ->  % List game request
	    Games = ?SERVER:list_games(),
	    ?TCP:sendHeader(Socket, 6), %List game answer
	    ?TCP:sendList(Socket, "String", Games),
	    recv_lobby(Player);

	7 -> % Host request
	    case ?TCP:readBoolean(Socket) of
		0 -> %false
		    Game = ?SERVER:create_game(Player),
		    ?TCP:sendHeader(Socket, 9), % Join answer
		    ?TCP:sendString(Socket, Player#player.name),
		    recv_gamelobby(Player, Game);
		_ ->
		    'loadgame()' % ladda spel

	    end;
	8 -> % Join request
	   GameName = ?TCP:readString(Socket),
	   case ?SERVER:get_game(GameName) of
		   [] ->
			  ?TCP:sendFailPacket(Socket, 2, Header); % FailPacket "Game does not exist" 

		  Game when is_list(Game) ->
			case ?GAMESRV:is_locked(Game#game.name) of
				true ->
					?TCP:sendFailPacket(Socket, 3, Header); % FailPacket "Game is locked"
				false ->
					?TCP:sendHeader(Socket, 9), % Join answer
	    				?TCP:sendString(Socket, GameName),
					?GAMESRV:player_join(GameName, Player),
					recv_gamelobby(Player, Game)
			end
	end;
	
	_Other ->
	    ?TCP:sendFailPacket(Socket, -1, Header), %Fail packet invalid state
	    throwPacket(Header, Socket)
    end.
%recv_lobby(Player).


recv_gamelobby(Player, Game) ->
    Socket = Player#player.socket,
    Header = ?TCP:readHeader(Socket),

    case Header of
	11 -> % Change civilization request
	    NewCiv = ?TCP:readString(Socket);
	_Other ->
	    ?TCP:sendFailPacket(Socket, -1, Header), %Fail packet invalid state
	    throwPacket(Header, Socket)

    end.
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
