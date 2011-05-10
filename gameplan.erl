-module(gameplan).
-include("config.hrl").
-compile(export_all).


% Arguments: Size of maps to be genererated, The current Game
% Generates a Terrain-map,
% Generates a empty Unit-map (tuples) with tilerecords containing their position
% Adds starting units to the Unit-map
% Updates the game-record with the new maps and returns the updated game record
make_gameplan(Size, Game) ->
    Players = Game#game.players,
    TerrainMatrix = ?TERGEN:generate(Size, Size),
    UnitMatrix = erlang:make_tuple(Size, erlang:make_tuple(Size, #tile{})),
    UpdUnitMatrix = add_pos(UnitMatrix, Size, Size, Size),
    NumberOfPlayers = length(Game#game.players),
    StartPos = ?START_POS:get_placement(NumberOfPlayers, TerrainMatrix),
    UpdatedUnitMatrix = assign_pos(Players, StartPos, UpdUnitMatrix),
    Game#game{map = TerrainMatrix, tilemap = UpdatedUnitMatrix}. 
    
% Arguments: List of Players to be assinged a starting position, List of positions {X, Y}, The Unit-map to be updated with units at starting positions
% Adds a unit (currently a Knight) on the positions, one unit for each player
% Returns the updated unit-map
assign_pos([], [], UMap) ->
    UMap;
assign_pos([Player|PlayerRest], [Pos|PosRest], UMap) ->
    UpdatedUMap = add_unit(UMap, Pos, "Knight", Player#player.name),
    assign_pos(PlayerRest, PosRest, UpdatedUMap).

% Arguments: Map to be updated, Tile to be inserted, Position of the tile to be replaced
% Replaces the tile at position X,Y with the new tile
% Returns the updated unit-map
update_tile(Map, Tile, X, Y) -> %Returns a updated version of the map
    UpdatedRow = setelement(Y, element(X, Map), Tile),
    setelement(X, Map, UpdatedRow).

% Arguments: Map to add unit to, Position where to add unit, Type of the unit, Owner of the unit,
% If the Position is vacant:
%   Creates a new tile with the unit and replaces the old tile,
%   Returns the updated unit-map
% Else
%   Returns {error, occupied}
create_unit(Map, {X, Y}, UnitType, Owner) -> %Adds a unit if the tile is vacant and returns a updated map, else returns {error, occupied}.
    case get_unit(Map, X, Y) of
	null ->
	    NewUnit = ?U_HANDLER:create_unit(UnitType, Owner),
	    OldTile = get_tile(Map, X, Y),
	    NewTile = OldTile#tile{unit=NewUnit},
	    update_tile(Map, NewTile, X, Y);
	_ ->
	    {error, occupied}    
    end.

remove_unit(Map, X, Y) ->
    Tile = get_tile(Map, X, Y),
    NewTile = Tile#tile{unit = null},
    update_tile(Map, NewTile, X, Y).

add_unit(Map,Unit, X, Y) ->
    Tile = get_tile(Map, X, Y),
    case get_unit(Map, X, Y) of
	null ->
	    NewTile = Tile#tile{unit=Unit},
	    {ok, update_tile(Map, NewTile, X, Y)};
	_ ->
	    {error, "Already a unit on tile"}
    end.

% Arguments: The map to extract the tile from, Position of the tile to be extracted
% Fetches the tile at position X,Y
get_tile(Map, X, Y) -> %Returns a tile
    element(Y, element(X, Map)).

%Returns the unit on the tile at position X,Y or null if there is no unit
get_unit(Map, X, Y) -> 
    Tile = element(Y, element(X, Map)),
    Tile#tile.unit.

tuplemap_to_listmap(Tuple) ->
	[tuple_to_list(Column)|| Column <- tuple_to_list(Tuple)].


% Arguments: UnitMap to update positions to, Size of X, Y, and original size of map (same as X and Y)
% Updates every tile#position in the unit-map according to the maps size
% Returns the updated unit-map
add_pos(UMap, 0, _Y, _OrigSize) ->
    UMap;
add_pos(UMap, X, 0, OrigSize) ->
    add_pos(UMap, X-1, OrigSize, OrigSize);
add_pos(UMap, X, Y, OrigSize) ->
    UpdatedTile = #tile{position = {X, Y}},
    UpdatedRow = setelement(Y, element(X, UMap), UpdatedTile),  
    UpdatedUMap = setelement(X, UMap, UpdatedRow),
    add_pos(UpdatedUMap, X, Y-1, OrigSize). 


make_move([{position, X, Y} | Tail], Game) ->
    case get_unit(Game#game.tilemap, X, Y) of
	null ->
	    {error, "No unit at starting position"};
	Unit ->
	    make_move(Tail, Game, Unit, {startpos, X, Y})
    end.

make_move([{position, _EX, _EY}|Tail], Game, Unit, Start) when length(Tail) =/= 0 ->
    make_move(Tail, Unit, Game, Start);
make_move([{position, EX, EY}], Game, Unit, {startpos, SX, SY}) ->
    Unitmap = Game#game.tilemap,
    NewUnitmap = remove_unit(Unitmap, SX, SY),
    case add_unit(NewUnitmap, Unit, EX, EY) of
	{ok, Map} -> {ok, Game#game{tilemap = Map}};
	{error, Reason} -> {error, Reason}
    end.
