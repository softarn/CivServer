-module(server).

-behaviour(gen_server).

-export([init/1]).

start(

init(Port) ->
    {ConHandler, _} = spawn_monitor(con_handler, init, [Port, self()]),
    loop([], []).

loop(Games, Players) ->
    receive
	{new_player, Player} ->
	    loop(Games, [Player|Players]);
	{list_games, Player} ->
	    GameNames = [Game#game.name || Game <- Games],
	    parser:send_games(Player#player.socket, GameNames)
    end.
