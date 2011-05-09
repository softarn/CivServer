-module(gameplan).
-include("config.hrl").
-compile(export_all).

make_gameplan(Size, Game) ->
    TerrainMatrix = ?TERGEN:generate(Size, Size),
    UnitMatrix = erlang:make_tuple(Size, erlang:make_tuple(Size, #tile)),
    numberOfPlayers = length(Game#game.players),
    StartPos = ?START_POS:get_placement(TerrainMatrix, numberOfPlayers).
    


get_unit({X, Y}) ->

