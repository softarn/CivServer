-module(gameserver).
-behaviour(gen_server).

-compile(export_all).
-export([start/2, init/1]).
-export([player_join/1]).

-include("config.hrl").

%Startup
start(Host) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Host, []).

init(Host}) ->
    Game = #game{name=Host#player.name, game_pid=self(), players=[Host]},
    ?SERVER:add_game(Game),
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

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

terminate(Reason, State) ->
    ok.

%Server calls and casts
list_players() -> gen_server:call(?MODULE, list_players).
toggle_lock() -> gen_server:call(?MODULE, toggle_lock).
player_join(Player) -> gen_server:call(?MODULE, {player_join, Player}).
stop() -> gen_server:call(?MODULE, stop).
