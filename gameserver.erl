-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/1, init/1]).
-export([player_join/1]).

-include("config.hrl").

%Startup
start(Host) ->
    Game = #game{name = Host#player.name, game_pid = self(), players = [Host]},
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
	Game#game.locked;

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

terminate(Reason, State) ->
    ok.

%Server calls and casts
list_players(GN) -> gen_server:call(GN, list_players).
toggle_lock(GN) -> gen_server:call(GN, toggle_lock).
is_locked(GN) -> gen_server:call(GN, is_locked).
player_join({GN, Player}) -> gen_server:call(GN, {player_join, Player}).
stop(GN) -> gen_server:call(GN, stop).
