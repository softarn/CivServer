-module(player_fsm).
-behaviour(gen_fsm).

-include("config.hrl").

-compile(export_all).
start() ->
    gen_fsm:start(?MODULE, [], []).

init([]) ->
    io:format("Inne i init"),
    {ok, connecting, []}. % 2. first state, 3: fsm-data

connecting(Player, _From, _StateData) ->
    io:format("Inne i connecting"),
    Pid = spawn_link(?P_HANDLER, init, [Player#player.socket, self()]), %skicka med pid
    NewPlayer = Player#player{handler_pid = Pid},
    {next_state, getting_p_info, NewPlayer}. %2: new state, 3: state-data

getting_p_info(Packet, Player) -> %{Header, List}, Player) ->
    io:format("inne i p_info"),
    {Header, List} = Packet,
    case Header of
	2 -> % Hello World
	    [ProtocolVersion, PlayerName] = List,

	    case ProtocolVersion =:= ?P_VERSION of

		true ->
		    UpdatedPlayer = Player#player{name=PlayerName},

		    case ?SERVER:add_player(UpdatedPlayer) of
			true ->
			    ?P_HANDLER:sendMsg(UpdatedPlayer#player.socket, {3, []}), %Welcome the the Real world
			    {next_state, server_lobby, UpdatedPlayer}; 
			false ->
			    ?P_HANDLER:sendFailMsg(UpdatedPlayer#player.socket, 1, Header) %Fail packet name already exists

		    end; %end add player

		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 0, Header) %Send fail packet, wrong protocolversion

	    end; %end protocolversion

	_ ->
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header)%Send fail packet, invalid state

    end. %end header

server_lobby({Header, List}, Player) ->
    case Header of
	5 -> % List game request
	    Games = ?SERVER:list_games(),
	    ?P_HANDLER:sendMsg(Player#player.socket, {6, [Games]}), %List game answer
	    {next_state, server_lobby, Player};
	7 -> % Host request
	    [LoadFlag] = List,

	    case LoadFlag of 
		0 -> %New game
		    Game = ?SERVER:create_game(Player),
		    HostName = Player#player.name,
		    ?P_HANDLER:sendMsg(Player#player.socket, {9, [HostName]}), %Join answer
		    {next_state, game_lobby, {Player, Game}};

		_ ->    %Load game, implement later
		    'loadgame()' % Ladda spel

	    end; %end header

	8 -> %Join request
	    [GameName] = List,

	    case ?SERVER:get_game(GameName) of
		[] ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 2, Header);% FailPacket "Game does not exist" 

		Game when is_record(Game, game) ->
		    case ?GAMESRV:is_locked(Game#game.name) of
			true ->
			    ?P_HANDLER:sendFailMsg(Player#player.socket, 3, Header); % FailPacket "Game is locked"
			false ->
			    ?P_HANDLER:sendMsg(Player#player.socket, {9, [GameName]}),
			    ?GAMESRV:player_join(GameName, Player),
			    {next_state, game_lobby, {Player, Game}}

		    end %end game is locked
	    end; %end get_game


	_ -> 
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header)
    end.

game_lobby({Header, List}, {Player, Game}) ->
    case Header of

	11 -> %Change civilization request
	    [NewCiv] = List,
	    UpdatedPlayer = Player#player{civ = NewCiv};
	    %Broadcasta till samtliga spelare i gamet om den nya civilizationen

	12 -> %Lock game request
	    [LockFlag] = List; %FIXA!!!

	13 -> %Start game request
	    case Player#player.name =:= Game#game.name of % Is player host?
		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 13, Header); % FailPacket "Permission denied"
		true ->
		    UpdatedGame = ?GAMESRV:start_game(Game#game.name, 30), % param: Gamename, mapsize
		    {next_state, in_game, {Player, UpdatedGame}}

	    end % player = host

    end. %end case header

in_game({Header, List}, {Player, Game}) ->
    ok.

terminate(Reason, StateName, StateData) ->
    io:format("~p~n", [Reason]).

%Event sending functions
connect(Pid, Player) ->
    gen_fsm:sync_send_event(Pid, Player). %1:Which FSM, 2:arg
send_packet(Pid, Packet) ->
    gen_fsm:send_event(Pid, Packet).
