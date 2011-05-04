-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/1, init/1]).
-export([player_join/2]).

-include("config.hrl").

%Startup
start(Host) ->
    Game = #game{name = Host#player.name, game_pid = self(), players = [Host], locked = 0},
    gen_server:start_link({local, list_to_atom(Host#player.name)}, ?MODULE, Game, []),
    Game.

init(Game) ->
    {ok, Game}.

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
    {stop, normal, shutdown_ok, State}.

terminate(Reason, State) ->
    ok.

%Server calls and casts
list_players(GN) -> gen_server:call(list_to_atom(GN), list_players).
toggle_lock(GN, LockFlag) -> gen_server:call(list_to_atom(GN), {toggle_lock, LockFlag}).
is_locked(GN) -> gen_server:call(list_to_atom(GN), is_locked).
start_game(GN, MapSize) -> gen_server:call(list_to_atom(GN), {start_game, MapSize}).
player_join(GN, Player) -> gen_server:call(list_to_atom(GN), {player_join, Player}).
stop(GN) -> gen_server:call(list_to_atom(GN), stop).

starting_game(Game) ->
    Players = Game#game.players,
    Fun = fun(X) ->
	    FSM = X#player.fsm_pid,
	    ?P_FSM:enter_game(FSM, Game)
    end,
    lists:foreach(Fun, Players).

broadcastMsg(Game, Msg, Type) ->
    Players = Game#game.players,	
    case Type of
	start_game ->
	    Fun = fun(X) -> 
		    Socket = X#player.socket,
		    ?TCP:sendHeader(Socket, 14), % Start game answer
		    ?TCP:sendList(Socket, "Column", Msg)
	    end,
	    lists:foreach(Fun, Players);

	game_info ->
	    GetPlayer = fun(X) ->
		    Name = X#player.name,
		    Civ = "Rabarber",%X#player.civ,
		    [Name, Civ]
	    end,
	    PList = [GetPlayer(X) || X <- Players],

	    Fun = fun(X) ->
		    Socket = X#player.socket,

		    ?TCP:sendHeader(Socket, 10), %Game session information
		    ?TCP:sendList(Socket, "Player", PList),
		    ?TCP:sendBoolean(Socket, Game#game.locked)				
	    end,
	    lists:foreach(Fun, Players)
    end.
