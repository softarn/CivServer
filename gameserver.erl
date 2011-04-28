-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start_link/2, init/2]).
-export([player_join/1]).

-include("config.hrl").

%Startup
start_link(Parent, Host) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, {Parent, Host}, []).

init({Parent, Host}) ->
	Game = #game{name=Host#player.name, game_pid=self(), players=[Host]},
	Parent ! Game, %add handle_info/2 i server... eller lös på annat sätt
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
	UpdatedGame = Game#game{players = lists:flatten([Player | Game#game.players])}, %Behövs lists:flatten? players = lista av players. 
    	io:format("Added player ~w~n", [Player]), %Glöm ej felkontroll ifall player existerar!
    	{reply, UpdatedGame#game.players, UpdatedGame};

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

terminate(Reason, State) ->
    %Kill con_handler?
    none.

%Server calls and casts
list_players() -> gen_server:call(?MODULE, list_players).
toggle_lock() -> gen_server:call(?MODULE, toggle_lock).
player_join(Player) -> gen_server:call(?MODULE, {player_join, Player}).
stop() -> gen_server:call(?MODULE, stop).
