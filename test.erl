-module(test).

-compile(export_all).
-include("config.hrl").

add_player(Name, Socket) ->
    ok = server:add_player(#player{name = Name,
	    socket = Socket}).

list_players() ->
    io:format("players: ~p~n", [server:list_players()]).

list_games() ->
    io:format("games: ~p~n", [server:list_games()]).

add_game(Game) ->
	server:add_game(Game).

