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
    io:format("In state getting_p_info  ~n"),
    case Header of

	2 -> % Hello World
	    [ProtocolVersion, PlayerName] = List,

	    case ProtocolVersion =:= ?P_VERSION of

		true ->
		    UpdatedPlayer = Player#player{name=PlayerName},
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
    io:format("In state server_lobby~n"),
    case Header of
	5 -> % List game request
	    Games = ?SERVER:list_games(),
	    ?P_HANDLER:sendMsg(Player#player.socket, {6, [Games]}), %List game answer
	    {next_state, server_lobby, {Player, Game}};

	7 -> % Host request
	    [LoadFlag] = List,

	    case LoadFlag of 
		0 -> %New game
		    NewGame = ?SERVER:create_game(Player),
		    HostName = Player#player.name,
		    ?P_HANDLER:sendMsg(Player#player.socket, {9, [HostName]}), %Join answer
		    {next_state, game_lobby, {Player, NewGame}};

		_ ->    %Load game, implement later
		    {next_state, server_lobby, {Player, Game}}
		    % Ladda spel
		    %Lägg in state senare...

	    end; %end header

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
    io:format("In state game_lobby~n"),
    case Header of

	11 -> %Change civilization request
	    [NewCiv] = List,
	    UpdatedPlayer = Player#player{civ = NewCiv},
	    {next_state, game_lobby, {UpdatedPlayer, Game}};
	%Broadcasta till samtliga spelare i gamet om den nya civilizationen

	12 -> %Lock game request
	    case Player#player.name =:= Game#game.name of
		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 13, Header), % FailPacket "Permission denied"
		    {next_state, game_lobby, {Player, Game}};
		true ->
		    [LockFlag] = List,
		    UpdatedGame = ?GAMESRV:toggle_lock(Game#game.game_pid, LockFlag),
		    {next_state, game_lobby, {Player, UpdatedGame}}
	    end;

	13 -> %Start game request
	    case Player#player.name =:= Game#game.name of % Is player host?
		false ->
		    ?P_HANDLER:sendFailMsg(Player#player.socket, 13, Header), % FailPacket "Permission denied"
		    {next_state, game_lobby, {Player, Game}};
		true ->
		    UpdatedGame = ?GAMESRV:start_game(Game#game.game_pid, 30), % param: Gamename, mapsize
		    {next_state, in_game, {Player, UpdatedGame}}

	    end;	% player = host
	
	_ -> %Other
	    ?P_HANDLER:sendFailMsg(Player#player.socket, -1, Header), % FailPacket "Permission denied"
	    {next_state, game_lobby, {Player, Game}}
	    
    end. %end case header

in_game({Header, List}, {Player, Game}) ->
    io:format("In state in_game: ~p~n", [Player#player.name]),
    io:format("Not implemented yet~n" ),
    {next_state, in_game, {Player, Game}}.

handle_event(NewState, StateName, {Player, Game}) ->
    case NewState of
	{enter_game, UpdatedGame} ->
	    {next_state, in_game, {Player, UpdatedGame}}
    end.
		
terminate(Reason, StateName, StateData) ->
    io:format("~p~n", [Reason]).

%Event sending functions
connect(Pid, Player) ->
    gen_fsm:send_event(Pid, Player). %1:Which FSM, 2:arg
send_packet(Pid, Packet) ->
    gen_fsm:send_event(Pid, Packet).
enter_game(Pid, UpdatedGame) ->
    gen_fsm:send_all_state_event(Pid, {enter_game, UpdatedGame}).
