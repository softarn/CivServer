-module(server).
-behaviour(gen_server).

-compile(export_all).
-export([start_link/1, init/1]).
-export([add_player/1]).

-include("config.hrl").

%Startup
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port, ?MODULE], []).

init([Port, Module]) ->
    spawn_link(con_handler, start, [Port, Module]),
    {ok, {[], []}}.

%Callbacks
handle_call(list_players, _From, {Games, Players}) ->
    {reply, [Player#player.name || Player <- Players], {Games,Players}};
handle_call(list_games, _From, {Games, Players}) ->
    {reply, [Game#game.name || Game <- Games], {Games,Players}};
handle_call({add_player, Player}, _From, {Games, Players}) ->
    io:format("Added player ~w~n", [Player]),
    {reply, ok, {Games, [Player|Players]}}.

%Server calls and casts
add_player(Player) -> gen_server:call(?MODULE, {add_player, Player}).
list_players() -> gen_server:call(?MODULE, list_players).
list_games() -> gen_server:call(?MODULE, list_games).
