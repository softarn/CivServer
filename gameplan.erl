-module(gameplan).
-include("config.hrl").
-compile(export_all).

make_gameplan(Size, Game) ->
    Players = Game#game.players,
    TerrainMatrix = ?TERGEN:generate(Size, Size),
    UnitMatrix = erlang:make_tuple(Size, erlang:make_tuple(Size, #tile{})),
    NumberOfPlayers = length(Game#game.players),
    StartPos = ?START_POS:get_placement(TerrainMatrix, NumberOfPlayers),
    UpdatedUnitMatrix = assign_pos(Players, StartPos, UnitMatrix),
    UpdatedGame = Game#game{map = TerrainMatrix, tilemap = UpdatedUnitMatrix},
    UpdatedGame.
    
assign_pos([], [], UMap) ->
    UMap;
assign_pos([Player|PlayerRest], [Pos|PosRest], UMap) ->
    UpdatedUMap = add_unit(UMap, Pos, "Knight", Player#player.name),
    assign_pos(PlayerRest, PosRest, UpdatedUMap).

add_unit(Map, {X, Y}, UnitType, Owner) -> %Adds a unit if the tile is vacant and returns a updated map, else returns {error, occupied}.
    case get_unit(Map, X, Y) of
	null ->
	    NewUnit = ?U_HANDLER:create_unit(UnitType, Owner),
	    OldTile = get_tile(Map, X, Y),
	    NewTile = OldTile#tile{unit=NewUnit},
	    UpdatedMap = update_tile(Map, NewTile, X, Y);
	_ ->
	    {error, occupied}    
    end.

update_tile(Map, Tile, X, Y) -> %Returns a updated version of the map
    UpdatedRow = setelement(Y, element(X, Map), Tile),
    setelement(X, Map, UpdatedRow).

get_tile(Map, X, Y) -> %Returns a tile
    element(Y, element(X, Map)).

get_unit(Map, X, Y) -> %Returns a Unit or null
    Tile = element(Y, element(X, Map)),
    Tile#tile.unit.
