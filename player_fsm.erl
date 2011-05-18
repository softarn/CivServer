-module(player_fsm).
-behaviour(gen_fsm).

-include("config.hrl").

-compile(export_all).
start() ->
    gen_fsm:start(?MODULE, [], []).

init([]) ->
    {ok, connecting, []}. % 2. first state, 3: fsm-data

connecting(Player, _StateData) ->
    Pid = spawn_link(?P_HANDLER, init, [Player#player.socket, self()]), %skicka med pid
    NewPlayer = Player#player{handler_pid = Pid},
    {next_state, getting_p_info, {NewPlayer, #game{}}}. %2: new state, 3: state-data

getting_p_info({Header, List}, {Player, Game}) -> 
    io:format("INNE I GETTING P INFO!!!~n"),
    case Header of

	2 -> % Hello World
	    [ProtocolVersion, PlayerName] = List,

	    case ProtocolVersion =:= ?P_VERSION of

		true ->
		    UpdatedPlayer = Player#player{name=PlayerName, civ = "DefaultCiv"},

		    case ?SERVER:add_player(UpdatedPlayer) of
			true ->
			    ?P_HANDLER:sendMsg(UpdatedPlayer#player.socket, {3, []}), %Welcome the the Real world
			    {next_state, server_lobby, {UpdatedPlayer, Game}}; 
			false ->
			    ?P_HANDLER:sendFailMsg(UpdatedPlayer#player.socket, 1, Header), %Fail packet name already exists
			    {next_state, getting_p_info, {Player, Game}}

		    end; %end add player

		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 0, Header), %Send fail packet, wrong protocolversion
		    {next_state, getting_p_info, {Player, Game}}

	    end; %end protocolversion

	_ ->
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header), %Send fail packet, invalid state
	    {next_state, getting_p_info, {Player, Game}}

    end. %end header

server_lobby({Header, List}, {Player, Game}) ->
    io:format("INNE I SERVER LOBBY~n"),
    case Header of
	5 -> % List game request
	    Games = ?SERVER:list_games(),
	    ?P_HANDLER:sendMsg(Player#player.socket, {6, [Games]}), %List game answer
	    {next_state, server_lobby, {Player, Game}};

	7 -> % Host request
	    NewGame = ?SERVER:create_game(Player),
	    case NewGame of
		failed ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 1, Header), %FailPacket "Name already exists"
		    {next_state, server_lobby, {Player, Game}};
		_ ->
		    HostName = Player#player.name,
		    ?P_HANDLER:sendMsg(Player#player.socket, {9, [HostName]}), %Join answer
		    JoinGame = ?GAMESRV:player_join(NewGame#game.game_pid, Player),
		    {next_state, game_lobby, {Player, JoinGame}}
	    end;

	8 -> %Join request
	    [GameName] = List,

	    case ?SERVER:get_game(GameName) of
		[] ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 2, Header),% FailPacket "Game does not exist" 
		    {next_state, server_lobby, {Player, Game}};

		ListedGame when is_record(ListedGame, game) ->
		    case ?GAMESRV:is_locked(ListedGame#game.game_pid) of
			0 ->
			    ?P_HANDLER:sendMsg(Player#player.socket, {9, [GameName]}),
			    JoinGame = ?GAMESRV:player_join(ListedGame#game.game_pid, Player),
			    {next_state, game_lobby, {Player, JoinGame}};
			_->
			    ?P_HANDLER:sendFailMsg(Player#player.socket, 3, Header), % FailPacket "Game is locked"
			    {next_state, server_lobby, {Player, Game}}

		    end %end game is locked
	    end; %end get_game

	_ -> 
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header),
	    {next_state, server_lobby, {Player, Game}}
    end.

game_lobby({Header, List}, {Player, Game}) ->
    io:format("INNE I GAME LOBBY~n"),
    case Header of

	11 -> %Change civilization request
	    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
	    [NewCiv] = List,
	    UpdatedPlayer = Player#player{civ = NewCiv},
	    UpdatedGame = ?GAMESRV:change_civ(Game#game.game_pid, UpdatedPlayer),
	    {next_state, game_lobby, {UpdatedPlayer, UpdatedGame}};
	%Broadcasta till samtliga spelare i gamet om den nya civilizationen

	12 -> %Lock game request
	    case Player#player.name =:= Game#game.name of
		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 4, Header), % FailPacket "Permission denied"
		    {next_state, game_lobby, {Player, Game}};
		true ->
		    [LockFlag] = List,
		    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
		    UpdatedGame = ?GAMESRV:toggle_lock(Game#game.game_pid, LockFlag),
		    {next_state, game_lobby, {Player, UpdatedGame}}
	    end;

	13 -> %Start game request
	    case Player#player.name =:= Game#game.name of % Is player host?
		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 4, Header), % FailPacket "Permission denied"
		    {next_state, game_lobby, {Player, Game}};
		true ->
		    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Comfirm'd
		    ?GAMESRV:start_game(Game#game.game_pid, 20), % param: Gamename, mapsize
		    {next_state, game_wait, {Player, Game}}

	    end;	% player = host

	24 -> %Exit game request
	    ?GAMESRV:player_leave(Game#game.game_pid, Player),
	    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
	    {next_state, server_lobby, {Player, null}};

	_ -> %Other
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header), % FailPacket "invalid state"
	    {next_state, game_lobby, {Player, Game}}

    end. %end case header

game_wait({Header, _List}, {Player, Game}) ->
    io:format("INNE I GAMEWAIT: ~p~n", [Player#player.name]),
    case Header of

	24 -> %Exit game request
	    ?GAMESRV:player_leave(Game#game.game_pid, Player),
	    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
	    {next_state, server_lobby, {Player, null}};

	_ ->
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header), % FailPacket "invalid state"
	    {next_state, game_wait, {Player, Game}}
    end.

game_turn({Header, List}, {Player, Game}) -> %GLÖM EJ ATT UPPDATERA GAME i början av varje turn
    io:format("INNE I GAMETURN: ~p~n", [Player#player.name]),
    case Header of

	15 -> %Move request
	    [PositionList] = List,
	    case ?GAMESRV:move_unit(Game#game.game_pid, PositionList) of
		{error, Reason} ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 6, Header), % FailPacket "Invalid move"
		    io:format("Error! ~p~n", [Reason]),
		    {next_state, game_turn, {Player, Game}};
		{ok, UpdatedGame} ->
		    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
		    {next_state, game_turn, {Player, UpdatedGame}}
	    end;
	16 -> %End of turn
	    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
	    UpdatedGame = ?GAMESRV:finished_turn(Game#game.game_pid),
	    {next_state, game_wait, {Player, UpdatedGame}};
	18 -> %Combat request
	    [{position, AttX, AttY}, {position, DefX, DefY}] = List,

	    case ?GAMESRV:attack_unit(Game#game.game_pid, {AttX, AttY}, {DefX, DefY}) of
		{ok, UpdatedGame, {RemAttMp, RemDefMp}} ->
		    ?P_HANDLER:sendMsg(Player#player.socket, {19, [RemAttMp, RemDefMp]}),
		    {next_state, game_turn, {Player, UpdatedGame}};
		{error, _Reason} ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 11, Header), %FailPacket "Out of range" PERHAPS INVALID TILE/POS/NO UNIT AT TILE DEPENDING ON FAILREASON?
		    {next_state, game_turn, {Player, Game}}
	    end;
	20 -> %Message for you sir
	    {next_state, game_turn, {Player, Game}};

	21 -> %Built city
	    [{position, X, Y}, CityName] = List,
	    case ?GAMESRV:build_city(Game#game.game_pid, {X, Y}, CityName, Player#player.name) of
		{ok, UpdatedGame} ->
		    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %confirmd
		    {next_state, game_turn, {Player, UpdatedGame}};
		{error, _Reason} ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 7, Header), % FailPacket "Invalid tile"
		    {next_state, game_turn, {Player, Game}}
	    end;
	    
	23 -> %Spawnd
	    [{position, X, Y}, {unit, Owner, UnitType, _Manpower}] = List,
	    case ?GAMESRV:create_unit(Game#game.game_pid, {X, Y}, UnitType, Owner) of
		{ok, UpdatedGame} ->
		    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
		    {next_state, game_turn, {Player, UpdatedGame}};
		{error, _Reason} ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 7, Header), % FailPacket "Invalid tile"
		    {next_state, game_turn, {Player, Game}}
	    end;

	24 -> %Exit game request
	    ?GAMESRV:player_leave(Game#game.game_pid, Player),
	    ?P_HANDLER:sendMsg(Player#player.socket, {1, [Header]}), %Confirm'd
	    {next_state, server_lobby, {Player, null}};

	_ ->
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header), % FailPacket "invalid state"
	    {next_state, game_turn, {Player, Game}}
    end.

handle_event(Msg, _StateName, {Player, _Game}) ->
    case Msg of
	{game_wait, UpdatedGame} ->
	    {next_state, game_wait, {Player, UpdatedGame}};

	game_close ->
	    {next_state, server_lobby, {Player, null}};

	{game_turn, UpdatedGame} ->
	    {next_state, game_turn, {Player, UpdatedGame}}
    end.

terminate(Reason, _StateName, _StateData) ->
    io:format("~p~n", [Reason]).

%Event sending functions
connect(Pid, Player) ->
    gen_fsm:send_event(Pid, Player). %1:Which FSM, 2:arg
send_packet(Pid, Packet) ->
    gen_fsm:send_event(Pid, Packet).
enter_game(Pid, UpdatedGame) ->
    gen_fsm:send_all_state_event(Pid, {game_wait, UpdatedGame}).
enter_turn(Pid, UpdatedGame) ->
    gen_fsm:send_all_state_event(Pid, {game_turn, UpdatedGame}).
game_close(Pid) ->
    gen_fsm:send_all_state_event(Pid, game_close).

