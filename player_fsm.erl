-module(player_fsm).
-behaviour(gen_fsm).

-include("config.hrl").

-compile(export_all).

start() ->
    gen_fsm:start(?MODULE, [], []).

init([]) ->
    {ok, connecting, []}. % 2. first state, 3: fsm-data

connecting(Player, _State) ->
    {ok, Pid} = spawn_link(?P_HANDLER, init, [Player#player.socket]), %skicka med pid
    NewPlayer = Player#player{handler_pid = Pid},
    {new_state, getting_p_info, NewPlayer}. %2: new state, 3: state-data

getting_p_info(Player, {Header, List}) ->
    case Header of
	2 -> % Hello World
	    [ProtocolVersion, PlayerName] = List,

	    case ProtocolVersion =:= ?P_VERSION of

		true ->
		    case ?SERVER:add_player(Player) of
			true ->
			    ?P_HANDLER:sendMsg(Player, {3, []}),
			    server_lobby(Player); %Welcome the the Real world
			false ->
			    ?P_HANDLER:sendFailMsg(Player, 1, Header) %Fail packet name already exists

		    end; %end add player

		false ->
		    ?P_HANDLER:sendFailMsg(Player, 0, Header) %Send fail packet, wrong protocolversion

	    end; %end protocolversion

	_ ->
	    ?P_HANDLER:sendFailMsg(Player, -1, Header)%Send fail packet, invalid state

    end. %end header

server_lobby(Player) ->
    case Header of
	5 -> % List game request
	    Games = ?SERVER:list_games(),
	    ?P_HANDLER:sendMsg(Player, {6, [Games]}); %List game answer

	7 -> % Host request
	    [LoadFlag] = List,

	    case LoadFlag of 
		0 -> %New game
		    Game = ?SERVER:create_game(),
		    HostName = Player#player.name,
		    ?P_HANDLER:sendMsg(Player, {9, [HostName]}), %Join answer
		    game_lobby(Player, Game);

		_ ->    %Load game, implement later
		    'loadgame()' % Ladda spel

	    end; %end header

	8 -> %Join request
	    [GameName] = List,

	    case ?SERVER:get_game(GameName) of
		[] ->
		    ?P_HANDLER:sendFailMsg(Player, 2, Header);% FailPacket "Game does not exist" 

		Game when is_record(Game, game) ->
		    case ?GAMESRV:is_locked(Game#game.name) of
			true ->
			    ?P_HANDLER:sendFailMsg(Player, 3, Header), % FailPacket "Game is locked"
			    server_lobby(Player);
			false ->
			    ?P_HANDLER:sendMsg(Player, {9, [GameName]}),
			    ?GAMESRV:player_join(GameName, Player),
			    game_lobby(Player, Game)

		    end %end game is locked
	    end; %end get_game


	_ -> 
	    ?P_HANDLER:sendFailMsg(Player, -1, Header),
    end.

game_lobby(Player, {Header, List}) ->
    case Header of
	13 -> %Start game request
	    case Player#player.name =:= Game#game.name of % Is player host?
		false ->
		    ?P_HANDLER:sendFailMsg(Player, 13, Header); % FailPacket "Permission denied"
		true ->
		    UpdatedGame = ?GAMESRV:start_game(Game#game.name, 30), % param: Gamename, mapsize
		    in_game(Player, UpdatedGame)

	    end % player = host

    end. %end case header

in_game(Player, Game) ->

    ok.
%Event sending functions
connect(Pid, Player) ->
    gen_fsm:sync_send_event(Pid, Player).
