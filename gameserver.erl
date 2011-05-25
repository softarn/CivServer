-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/1, init/1]).
-export([player_join/2]).

-include("config.hrl").

%Startup
start(Host) ->
    Game = #game{name = Host#player.name, players = [], locked = 0, current_state = game_lobby}, %tog bort host fårn players
    {ok, Pid} = gen_server:start(?MODULE, Game, []),
    NewGame = Game#game{game_pid=Pid},
    NewGame.

init(Game) ->
    NewGame = Game#game{game_pid = self()},
    {ok, NewGame}.

%%%Callbacks


%Returns a list containing all the player names in the game
handle_call(list_players, _From, Game) ->
    {reply, [Player#player.name || Player <- Game#game.players], Game};


% Arguments: LockFlag - 0 means unlocked, everything else means locked
% If the LockFlag is different from current lock-status:
%   Change the lock,
%   Update the main server about the changes
%   Broadcast the "Game Session Information" packet to all the game's players
%   Returns the updated game record
%Else if the LockFlag isn't different from current lock-status
%   Return the Game-record (do nothing).  
handle_call({toggle_lock, LockFlag}, _From, Game) ->
    if 
	(Game#game.locked > 0) and (LockFlag > 0) ->
	    {reply, Game, Game};
	(Game#game.locked =:= 0) and (LockFlag =:= 0) ->
	    {reply, Game, Game};
	true -> %else..
	    UpdatedGame = Game#game{locked = LockFlag},
	    update_game(UpdatedGame),
	    {reply, UpdatedGame, UpdatedGame}
    end;

% Arguments: Player-record,
% Replaces the old player with the updated player (with the new civilization) to the games playerlist,
% Updates the main server about the changes
% Broadcasts the "Game Session Information" packet to all the game's players
% Returns the updated game record
handle_call({change_civ, UpdatedPlayer}, _From, Game) ->
   Rem_Player = fun(PL) -> %Rem_Player removes the player with the same name as UpdatedPlayer
	   PL#player.name =/= UpdatedPlayer#player.name
   end,

   Updated_Player_List = [UpdatedPlayer | lists:filter(Rem_Player, Game#game.players)], %adds the updated player to the result of lists:filter with Rem_Player
   UpdatedGame = Game#game{players = Updated_Player_List},
   update_game(UpdatedGame), 
   {reply, UpdatedGame, UpdatedGame};


% Arguments: The Player record to join the game,
% Adds the player to the game's playerlist
% Updates the main server about the changes
% Broadcasts the "Game Session Information" packet to all the game's players
% Returns the updated game record
handle_call({player_join, Player}, _From, Game) ->
    UpdatedGame = Game#game{players = [Player | Game#game.players]}, %
    io:format("Added player ~p~n", [Player#player.name]), %Glöm ej felkontroll ifall player existerar!
    update_game(UpdatedGame),
    {reply, UpdatedGame, UpdatedGame};

% Returns the game records locked status (0 = unlocked, everything else = locked)
handle_call(is_locked, _From, Game) ->
    {reply, Game#game.locked, Game};


handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

% Takes the first element of the game's playerlist (the current player) and puts it last in the list,
% Changes the next players state to "game_turn",
% Returns the update game record
handle_call(finished_turn, _From, Game) ->
   UpdatedGame = change_turn(Game), %Nästa spelares tur
   {reply, UpdatedGame, UpdatedGame};

handle_call({move_unit, PosList}, _From, Game) ->
    case ?GAMEPLAN:make_move(PosList, Game) of
	{ok, UpdatedGame} ->
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({create_unit, {X, Y}, UnitType, Owner}, _From, Game) ->
    case ?GAMEPLAN:create_unit(Game#game.tilemap, {X, Y}, UnitType, Owner) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({build_city, {X, Y}, CityName, CityOwner}, _From, Game) ->
    case ?GAMEPLAN:build_city(Game#game.tilemap, {X, Y}, CityName, CityOwner) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({insert_unit, {FX, FY}, {TX, TY}}, _From, Game) ->
    case ?GAMEPLAN:insert_unit(Game#game.tilemap, {FX, FY}, {TX, TY}) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({extract_unit, {CX, CY}, UnitType, MP, {TX, TY}}, _From, Game) ->
    case ?GAMEPLAN:extract_unit(Game#game.tilemap, {CX, CY}, UnitType, MP, {TX, TY}) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({disband_unit, {X, Y}, Owner}, _From, Game) ->
    case ?GAMEPLAN:disband_unit(Game#game.tilemap, {X, Y}, Owner) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({fortify_unit, {X, Y}, Owner}, _From, Game) ->
    case ?GAMEPLAN:fortify_unit(Game#game.tilemap, {X, Y}, Owner) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({unfortify_unit, {X, Y}, Owner}, _From, Game) ->
    case ?GAMEPLAN:unfortify_unit(Game#game.tilemap, {X, Y}, Owner) of
	{ok, UpdatedUnitMap} ->
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    {reply, {ok, UpdatedGame}, UpdatedGame};
	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end;

handle_call({attack_unit, {AttX, AttY}, {DefX, DefY}}, _From, Game) ->

    
    case ?GAMEPLAN:attack_unit(Game#game.tilemap, Game#game.map, {AttX, AttY}, {DefX, DefY}) of
	{ok, UpdatedUnitMap, {RemAttMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}} ->
	    FindPlayerFun = fun(PR) ->
		    PR#player.name =:= VictimStr
	    end,
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    VictimList = lists:filter(FindPlayerFun, Game#game.players),
	    if (VictimList =/= []) ->
		    Victim = hd(VictimList),
		    io:format("Sent a casualty report to ~p~n", [Victim#player.name]),
		    ?P_HANDLER:sendMsg(Victim#player.socket, {30, [{DefX, DefY}, DefMpLost]});
		true ->
		    ok
	    end,
	    {reply, {ok, UpdatedGame, {RemAttMp, RemDefMp}}, UpdatedGame};

	{bombardment, UpdatedUnitMap, {X, Y}, DPU, VictimStr} ->
	    FindPlayerFun = fun(PR) ->
		    PR#player.name =:= VictimStr
	    end,
	    UpdatedGame = Game#game{tilemap = UpdatedUnitMap},
	    VictimList = lists:filter(FindPlayerFun, Game#game.players),
	    if (VictimList =/= []) ->
		    Victim = hd(VictimList),
		    ?P_HANDLER:sendMsg(Victim#player.socket, {30, [{X, Y}, DPU]});
		true ->
		    ok
	    end,
	    {reply, {bombardment, UpdatedGame}, UpdatedGame};

	{error, Reason} ->
	    {reply, {error, Reason}, Game}
    end.

terminate(_Reason, _State) ->
    ok.

%% If the leaving player is host and game is in game_lobby:
    %Removes the player that left from the game,
    %Broadcasts Packet 25 (Game closed) to the remaining players and puts their state into "server_lobby"
    %Tells the main server to remove this game
    %Kills itself
%%Else
    %Removes the leaving player
    %If the game is empty
	%tells the main server to remove the game and kills itself
    %Else broadcasts game session information.
handle_cast({player_leave, Player}, Game) ->
    case (Player#player.name =:= Game#game.name) and (Game#game.current_state =:= game_lobby) of
	true ->
	    UpdatedGame = Game#game{players = Game#game.players -- [Player]},
	    broadcastMsg(UpdatedGame, game_close),
	    ?SERVER:remove_game(UpdatedGame),
	    io:format("Shutting down game after ~p left~n",[Player#player.name]),
	    {stop, normal, UpdatedGame};
	false ->
	    if
		(Game#game.current_state =:= in_game) and (hd(Game#game.players) =:= Player) -> % In_game and current turn = player that left
		    if
			(length(Game#game.players) =/= 1) ->
			    NextPlayerTurnGame = change_turn(Game), %Change turn
			    UpdatedGame = NextPlayerTurnGame#game{players = NextPlayerTurnGame#game.players -- [Player]}; %Remove player
			true ->
			   UpdatedGame = Game#game{players = []}
		    end;

		true ->
		    UpdatedGame = Game#game{players = Game#game.players -- [Player]}

	    end,

	    case UpdatedGame#game.players of
		[] -> %Game is empty
		    ?SERVER:remove_game(UpdatedGame),
		    io:format("Shutting down game after ~p left~n",[Player#player.name]),
		    {stop, normal, UpdatedGame};
		_ ->
		    update_game(UpdatedGame),
		    {noreply, UpdatedGame}
	    end
    end;

% Updates the game-record with a terrainmap, unitmap and starting units,
% Sets game status to locked and in_game,
% updates main server, puts players fsm into correct state (see starting_game comments below)
% Returns the updated game record
handle_cast({start_game, Width, _Height}, Game) ->
    if
	Width < 20 -> NewWidth = 20;
	Width > 100 -> NewWidth = 100;
	true -> NewWidth = Width
    end,
    UpdatedGame = ?GAMEPLAN:make_gameplan(NewWidth, NewWidth, Game), % Fixa storleken senare
    UpdatedGame2 = UpdatedGame#game{locked = 1, current_state = in_game},
    UpdatedGame3 = starting_game(UpdatedGame2),
    {noreply, UpdatedGame3}.

%Server calls and casts
list_players(Game_pid) -> gen_server:call(Game_pid, list_players).
toggle_lock(Game_pid, LockFlag) -> gen_server:call(Game_pid, {toggle_lock, LockFlag}).
change_civ(Game_pid, Player) -> gen_server:call(Game_pid, {change_civ, Player}).
player_join(Game_pid, Player) -> gen_server:call(Game_pid, {player_join, Player}).
is_locked(Game_pid) -> gen_server:call(Game_pid, is_locked).
stop(Game_pid) -> gen_server:call(Game_pid, stop).
finished_turn(Game_pid) -> gen_server:call(Game_pid, finished_turn).
move_unit(Game_pid, PosList) -> gen_server:call(Game_pid, {move_unit, PosList}).
create_unit(Game_pid, {X,Y}, UnitType, Owner) -> gen_server:call(Game_pid, {create_unit, {X, Y}, UnitType, Owner}).
build_city(Game_pid, {X,Y}, CityName, CityOwner) -> gen_server:call(Game_pid, {build_city, {X,Y}, CityName, CityOwner}).
insert_unit(Game_pid, {FX, FY}, {TX, TY}) -> gen_server:call(Game_pid, {insert_unit, {FX, FY}, {TX, TY}}).
extract_unit(Game_pid, {CX, CY}, UnitType, MP, {TX, TY}) -> gen_server:call(Game_pid, {extract_unit, {CX, CY}, UnitType, MP, {TX, TY}}).
disband_unit(Game_pid, {X, Y}, Owner) -> gen_server:call(Game_pid, {disband_unit, {X, Y}, Owner}).
fortify_unit(Game_pid, {X, Y}, Owner) -> gen_server:call(Game_pid, {fortify_unit, {X, Y}, Owner}).
unfortify_unit(Game_pid, {X, Y}, Owner) -> gen_server:call(Game_pid, {unfortify_unit, {X, Y}, Owner}).
attack_unit(Game_pid, {AttX, AttY}, {DefX, DefY}) -> gen_server:call(Game_pid, {attack_unit, {AttX, AttY}, {DefX, DefY}}).
player_leave(Game_pid, Player) -> gen_server:cast(Game_pid, {player_leave, Player}).
start_game(Game_pid, Width, Height) -> gen_server:cast(Game_pid, {start_game, Width, Height}).


%Internal functions

% Updates the main server with the game-status,
% broadcasts the "Start game answer" packet to the game's players (this includes sending the terrainmap and unitmap),
% puts all the game's player's FSM-states into the "game_wait" state
% puts the second first player in the playerlist into the "game_turn" state
% Returns the updatedgame record (with the updated playerlist from change_turn).
starting_game(Game) ->
    ?SERVER:update_game(Game),
    broadcastMsg(Game, start_game),
    Players = Game#game.players,
    Fun = fun(X) ->
	    FSM = X#player.fsm_pid,
	    ?P_FSM:enter_game(FSM, Game)
    end,
    lists:foreach(Fun, Players),
    change_turn(Game).


% Arguments: The current game-record,
% Puts the head of the playerlist last and updates the game with the new list,
% Takes the first player from the updated playerlist, sends its your turn and changes its current FSM-state to "game_turn".
% Returns the updated game record
change_turn(Game) ->

   [ToBeLast | Rest] = Game#game.players,

   case length(Game#game.players) of %Sätt aktuell spelare sist i listan
       1 -> 
	   UpdatedPlayers = Game#game.players; 
       _ ->
	   UpdatedPlayers = Rest ++ [ToBeLast]
   end, 
    UpdatedGame = Game#game{players = UpdatedPlayers},
    ToBeNext = hd(UpdatedPlayers),
    FSM = ToBeNext#player.fsm_pid,
    ?P_HANDLER:sendMsg(ToBeNext#player.socket, {17, [UpdatedGame#game.tilemap]}), %It's your turn
    ?P_FSM:enter_turn(FSM, UpdatedGame),
    io:format("It's now ~p's turn", [ToBeNext#player.name]),
    UpdatedGame.

% Updates the main server,
% Broadcasts the "Game Session Information" packet to all players in the game
update_game(Game) ->
    ?SERVER:update_game(Game),
    broadcastMsg(Game, game_info).


% If type is:
    % start_game:
	%sends "start game answer" + terrainmap + unitlist to all players in game, 
    % game_close:
	%sends "game closed" to all players in game,
	%puts all players fsm into "server_lobby" state, setting their game-record to null
    % game_info:
	%sends "Game session information" (with player name + player civilization) to all clients in the game
broadcastMsg(Game, Type) ->
    Players = Game#game.players,	
    case Type of
	start_game ->
	    Fun = fun(X) -> 
		    Socket = X#player.socket,
		    ?P_HANDLER:sendMsg(Socket, {14, [Game#game.map, Game#game.tilemap]}), % Start game answer
		    io:format("Skickade kartan till ~p~n", [X#player.name])
	    end,
	    lists:foreach(Fun, Players);

	game_close ->
	    Fun = fun(X) -> 
		    Socket = X#player.socket,
		    ?P_HANDLER:sendMsg(Socket, {25, []}), % Game Closed
		    ?P_FSM:game_close(X#player.fsm_pid)
	    end,
	    lists:foreach(Fun, Players);

	game_info ->
	    GetPlayer = fun(X) ->
		    Name = X#player.name,
		    Civ = X#player.civ,
		    {Name, Civ}
	    end,
	    PList = [GetPlayer(X) || X <- Players],

	    Fun = fun(X) ->
		    Socket = X#player.socket,
		    ?P_HANDLER:sendMsg(Socket, {10, [PList, Game#game.locked]}) %Game session information)
	    end,
	    lists:foreach(Fun, Players)
    end.
