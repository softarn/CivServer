-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/1, init/1]).
-export([player_join/1]).

-include("config.hrl").

%Startup
start(Host) ->
    Game = #game{name = Host#player.name, game_pid = self(), players = [Host], locked = false},
    gen_server:start_link({local, list_to_atom(Host#player.name)}, ?MODULE, Game, []),
    Game.

init(Game) ->
    {ok, Game}.

%Callbacks
handle_call(list_players, _From, Game) ->
    {reply, [Player#player.name || Player <- Game#game.players], Game};

handle_call(toggle_lock, _From, Game) ->
    case Game#game.locked of
	true ->
	    UpdatedGame = Game#game{locked = false};
	false ->
	    UpdatedGame = Game#game{locked = true}
    end,
    {reply, UpdatedGame#game.locked, UpdatedGame};

handle_call({player_join, Player}, _From, Game) ->
    UpdatedGame = Game#game{players = [Player | Game#game.players]}, %
    io:format("Added player ~w~n", [Player]), %Glöm ej felkontroll ifall player existerar!
    {reply, UpdatedGame#game.players, UpdatedGame};

handle_call(is_locked, _From, Game) ->
    {reply, Game#game.locked, Game};

handle_call({start_game, MapSize}, _From, Game) ->
    Map = ?TERGEN:generate(MapSize, MapSize), % Fixa storleken senare
    broadcastMsg(Game, Map, start_game),
    UpdatedGame = Game#game{locked = true, map = Map},
    {reply, UpdatedGame, UpdatedGame};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

terminate(Reason, State) ->
    ok.

%Server calls and casts
list_players(GN) -> gen_server:call(list_to_atom(GN), list_players).
toggle_lock(GN) -> gen_server:call(list_to_atom(GN), toggle_lock).
is_locked(GN) -> gen_server:call(list_to_atom(GN), is_locked).
start_game(GN, MapSize) -> gen_server:call(list_to_atom(GN), {start_game, MapSize}).
player_join({GN, Player}) -> gen_server:call(list_to_atom(GN), {player_join, Player}).
stop(GN) -> gen_server:call(list_to_atom(GN), stop).

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
	    Fun = fun(X) ->
		    Socket = X#player.socket,
		    ?TCP:sendHeader(Socket, 10),
		    ?TCP:sendList(Socket, Msg),
		    ?TCP:sendBoolean(Socket, Game#game.locked)				
	    end,
	    lists:foreach(Fun, Players)
    end.
