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

%Callbacks
handle_call(list_players, _From, Game) ->
    {reply, [Player#player.name || Player <- Game#game.players], Game};

handle_call({toggle_lock, LockFlag}, _From, Game) ->
    if 
	(Game#game.locked > 0) and (LockFlag > 0) ->
	    {reply, Game#game.locked, Game};
	(Game#game.locked =:= 0) and (LockFlag =:= 0) ->
	    {reply, Game#game.locked, Game};
	true -> %else..
	    UpdatedGame = Game#game{locked = LockFlag},
	    update_game(UpdatedGame),
	    {reply, UpdatedGame, UpdatedGame}
    end;

handle_call({change_civ, UpdatedPlayer}, _From, Game) ->
   Rem_Player = fun(PL) -> %Rem_Player removes the player with the same name as UpdatedPlayer
	   PL#player.name =/= UpdatedPlayer#player.name
   end,

   Updated_Player_List = [UpdatedPlayer | lists:filter(Rem_Player, Game#game.players)], %adds the updated player to the result of lists:filter with Rem_Player
   UpdatedGame = Game#game{players = Updated_Player_List},
   update_game(UpdatedGame), 
   {reply, UpdatedGame, UpdatedGame};

handle_call({player_join, Player}, _From, Game) ->
    UpdatedGame = Game#game{players = [Player | Game#game.players]}, %
    io:format("Added player ~w~n", [Player]), %Glöm ej felkontroll ifall player existerar!
    update_game(UpdatedGame),
    {reply, UpdatedGame, UpdatedGame};

handle_call(is_locked, _From, Game) ->
    {reply, Game#game.locked, Game};


handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call({finished_turn, Player}, _From, Game) ->
   [Current | Rest] = Game#game.players,
   case Current =:= Player of
       true ->
	   io:format("Current == Player");
       false ->
	   io:format("finished turn tog ut felaktig person! Current != player!")
   end,
   UpdatedPlayers = Rest ++ Current, %Sätt aktuell spelare sist i listan
   UpdatedGame = Game#game{players = UpdatedPlayers},
   change_turn(UpdatedGame), %Nästa spelares tur
   {reply, UpdatedGame, UpdatedGame}.

terminate(Reason, State) ->
    ok.

%player_leave:
%% If the leaving player is host and game is in game_lobby:
    %Removes the player that left from the game,
    %Broadcasts Packet 25 (Game closed) to the remaining players
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
	    {stop, "Host left the game", UpdatedGame};
	false ->
	    UpdatedGame = Game#game{players = Game#game.players -- [Player]},
	    case UpdatedGame#game.players of
		[] -> %Game is empty
		    ?SERVER:remove_game(UpdatedGame),
		    {stop, "All players have left - game is empty", UpdatedGame};
		_ ->
		    update_game(UpdatedGame),
		    {noreply, UpdatedGame}
	    end
    end;

handle_cast({start_game, MapSize}, Game) ->
    UpdatedGame = ?GAMEPLAN:make_gameplan(MapSize, Game), % Fixa storleken senare
    %ListUMap = ?GAMEPLAN:tuplemap_to_listmap(UpdatedGame#game.tilemap),
    UpdatedGame2 = UpdatedGame#game{locked = 1, current_state = in_game}, %Glöm ej att göra t_map till list
    starting_game(UpdatedGame2),
    {noreply, UpdatedGame2}.

%Server calls and casts
list_players(Game_pid) -> gen_server:call(Game_pid, list_players).
toggle_lock(Game_pid, LockFlag) -> gen_server:call(Game_pid, {toggle_lock, LockFlag}).
change_civ(Game_pid, Player) -> gen_server:call(Game_pid, {change_civ, Player}).
player_join(Game_pid, Player) -> gen_server:call(Game_pid, {player_join, Player}).
is_locked(Game_pid) -> gen_server:call(Game_pid, is_locked).
stop(Game_pid) -> gen_server:call(Game_pid, stop).
finished_turn(Game_pid, Player) -> gen_server:call(Game_pid, {finished_turn, Player}).
player_leave(Game_pid, Player) -> gen_server:cast(Game_pid, {player_leave, Player}).
start_game(Game_pid, MapSize) -> gen_server:cast(Game_pid, {start_game, MapSize}).

%Internal functions
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

change_turn(Game) ->
    [First | _Rest] = Game#game.players,
    FSM = First#player.fsm_pid,
    ?P_FSM:enter_turn(FSM, Game).

update_game(Game) ->
    ?SERVER:update_game(Game),
    broadcastMsg(Game, game_info).

broadcastMsg(Game, Type) ->
    Players = Game#game.players,	
    case Type of
	start_game ->
	    Fun = fun(X) -> 
		    Socket = X#player.socket,
		    TileList = lists:flatten(?GAMEPLAN:tuplemap_to_listmap(Game#game.tilemap)), %Gör om tuplemappen till en lista och skicka
		    ?P_HANDLER:sendMsg(Socket, {14, [Game#game.map, TileList]}) % Start game answer%DONT FORGET TO SEND LIST<TILE> WITH PRESET UNITS!
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
