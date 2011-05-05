-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/1, init/1]).
-export([player_join/2]).

-include("config.hrl").

%Startup
start(Host) ->
    Game = #game{name = Host#player.name, players = [Host], locked = 0},
    Pid = gen_server:start(?MODULE, Game, []),
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
	    broadcastMsg(UpdatedGame, UpdatedGame#game.players, game_info), 
	    {reply, UpdatedGame#game.locked, UpdatedGame}
    end;

handle_call({player_join, Player}, _From, Game) ->
    UpdatedGame = Game#game{players = [Player | Game#game.players]}, %
    io:format("Added player ~w~n", [Player]), %Glöm ej felkontroll ifall player existerar!
    broadcastMsg(UpdatedGame, UpdatedGame#game.players, game_info),
    {reply, UpdatedGame#game.players, UpdatedGame};

handle_call(is_locked, _From, Game) ->
    {reply, Game#game.locked, Game};

handle_call({start_game, MapSize}, _From, Game) ->
    Map = ?TERGEN:generate(MapSize, MapSize), % Fixa storleken senare
    UpdatedGame = Game#game{locked = 1, map = Map},
    starting_game(UpdatedGame),
    broadcastMsg(UpdatedGame, Map, start_game),
    {reply, UpdatedGame, UpdatedGame};

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

%Server calls and casts
list_players(Game) -> gen_server:call(Game, list_players).
toggle_lock(Game, LockFlag) -> gen_server:call(Game, {toggle_lock, LockFlag}).
is_locked(Game) -> gen_server:call(Game, is_locked).
start_game(Game, MapSize) -> gen_server:call(Game, {start_game, MapSize}).
player_join(Game, Player) -> gen_server:call(Game, {player_join, Player}).
stop(Game) -> gen_server:call(Game, stop).
finished_turn(Game, Player) -> gen_server:call(Game, {finished_turn, Player}).

starting_game(Game) ->
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

broadcastMsg(Game, Msg, Type) ->
    Players = Game#game.players,	
    case Type of
	start_game ->
	    Fun = fun(X) -> 
		    Socket = X#player.socket,
		    ?P_HANDLER:sendMsg(Socket, {14, [Msg]}) % Start game answer%DONT FORGET TO SEND LIST<TILE> WITH PRESET UNITS!
	    end,
	    lists:foreach(Fun, Players);

	game_info ->
	    GetPlayer = fun(X) ->
		    Name = X#player.name,
		    Civ = "Rabarber",%X#player.civ,
		    {Name, Civ}
	    end,
	    PList = [GetPlayer(X) || X <- Players],

	    Fun = fun(X) ->
		    Socket = X#player.socket,
		    ?P_HANDLER:sendMsg(Socket, {10, [PList, Game#game.locked]})
	    end,
	    lists:foreach(Fun, Players)
    end.
