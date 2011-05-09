-module(gameplan).
-include("config.hrl").
-compile(export_all).

make_gameplan(Size, Game) ->
    Players = Game#game.players,
    TerrainMatrix = ?TERGEN:generate(Size, Size),
    UnitMatrix = erlang:make_tuple(Size, erlang:make_tuple(Size, #tile)),
    NumberOfPlayers = length(Game#game.players),
    StartPos = ?START_POS:get_placement(TerrainMatrix, NumberOfPlayers).
    
assing_pos(Players, Positions, UMap) ->
    UMap.


add_unit(Map, {X, Y}, UnitType, Owner) ->
    case get_unit(Map, X, Y) of
	null ->
	    NewTile = get_tile(Map, X, Y)#tile{unit=
	    UpdatedMap = 
	Unit ->
	    "Tile occupied"	    
    

update_tile(Map, Tile, X, Y) -> %Returns a updated version of the map
    UpdatedRow = setelement(Y, element(X, Map), Tile),
    setelement(X, Map, UpdatedRow).

get_tile(Map, X, Y) -> %Returns a tile
    element(Y, element(X, Map)).

get_unit(Map, X, Y) -> %Returns a Unit or null
    Tile = element(Y, element(X, Map)),
    Tile#tile.unit.
