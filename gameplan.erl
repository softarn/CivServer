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
    {ok, UpdatedUMap} = create_unit(UMap, Pos, "Knight", Player#player.name),
    assign_pos(PlayerRest, PosRest, UpdatedUMap).

% Arguments: Map to be updated, Tile to be inserted, Position of the tile to be replaced
% Replaces the tile at position X,Y with the new tile
% Returns the updated unit-map
update_tile(Map, Tile, X, Y) -> %Returns a updated version of the map
    UpdatedRow = setelement(Y, element(X, Map), Tile),
    setelement(X, Map, UpdatedRow).

% Arguments: Map to be updated, Unit to be updated, Pos of unit
% If unit does not exist returns {error, Reason},
% Else if Pos of unit is out of bounds returns {error, Reason}
% Else returns {ok, UpdatedUnitMap} with the new unit at the position
update_unit(Map, Unit, X, Y) ->
    case get_unit(Map, X, Y) of
	null ->
	    {error, "Invalid tile"};
	{error, Reason} ->
	    {error, Reason};
	_OldUnit ->
	    case get_tile(Map, X, Y) of
		{ok, OldTile} ->
		    NewTile = OldTile#tile{unit = Unit},
		    {ok, update_tile(Map, NewTile, X, Y)};
		{error, Reason} ->
		    {error, Reason}
	    end
    end.

% Arguments: Map to add unit to, Position where to add unit, Type of the unit, Owner of the unit,
% If the Position is vacant:
%   Creates a new tile with the unit and replaces the old tile,
%   Returns the updated unit-map
% Else
%   Returns {error, occupied}
create_unit(Map, {X, Y}, UnitType, Owner) -> %Adds a unit if the tile is vacant and returns a updated map, else returns {error, occupied}.
    case get_unit(Map, X, Y) of
	null ->
	    case ?U_HANDLER:create_unit(UnitType, Owner) of
		{error, invalid_unit} ->
		    {error, "Invalid unit type"};
		NewUnit ->
		    case get_tile(Map, X, Y) of
			{ok, OldTile} ->
			    NewTile = OldTile#tile{unit=NewUnit},
			    io:format("Created a ~p at {~p,~p}~n", [UnitType, X, Y]),
			    {ok, update_tile(Map, NewTile, X, Y)};
			{error, Reason} ->
			    {error, Reason}
		    end
	    end;
	_ ->
	    {error, "Invalid tile"}    
    end.
% Arguments: The map in which to remove a unit, Pos of the tile
% Removes a unit from the tile at the given position
% Returns {ok, updated unit map}
remove_unit(Map, X, Y) ->
    case get_tile(Map, X, Y) of
	{ok, Tile} ->
	    NewTile = Tile#tile{unit = null},
	    {ok, update_tile(Map, NewTile, X, Y)};
	{error, Reason} ->
	    {error, Reason}
    end.

% Arguments: Map, Unit to be added, position
% Adds a unit to the position if the position is vacant, returns {ok, Updated-unit-map}
% If the position is occupied - returns {error, "Already a unit on tile"}
add_unit(Map,Unit, X, Y) ->
    case get_tile(Map, X, Y) of
	{ok, Tile} ->
	    case get_unit(Map, X, Y) of
		null ->
		    NewTile = Tile#tile{unit=Unit},
		    {ok, update_tile(Map, NewTile, X, Y)};
		_ ->
		    {error, "Invalid tile"}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.




% Arguments: The map to extract the tile from, Position of the tile to be extracted
% Fetches the tile at position X,Y if it is in the map, else returns {error, Reason}
%Returns {ok, Tile}
get_tile(Map, X, Y) ->
    if 
	(X > size(Map)) or (Y > size(element(1, Map))) or
	(X < 1) or (Y < 1)		->
	    {error, "Out of bounds"};
	true -> %else..
	    {ok, element(Y, element(X, Map))}
    end.

%If the X and Y is in the map-range -Returns the unit on the tile at position X,Y or null if there is no unit
% Else returns {error, Reason}
get_unit(Map, X, Y) -> 
    if 
	(X > size(Map)) or (Y > size(element(1, Map))) or
	(X < 1) or (Y < 1)		->
	    {error, "Out of bounds"};
	true -> %else..
	    Tile = element(Y, element(X, Map)),
	    Tile#tile.unit
    end.

get_city(Map, X, Y) -> 
    if 
	(X > size(Map)) or (Y > size(element(1, Map))) or
	(X < 1) or (Y < 1)		->
	    {error, "Out of bounds"};
	true -> %else..
	    Tile = element(Y, element(X, Map)),
	    Tile#tile.city
    end.

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

% Arguments: Next position in the list, The current game
% Checks if there is a unit at the starting position and returns {error, Reason} if there isnt, else returns {ok, UpdatedGame}
make_move([{position, X, Y} | Tail], Game) ->
    case get_unit(Game#game.tilemap, X, Y) of
	{error, Reason} ->
	    {error, Reason};
	null ->
	    {error, "No unit at starting position"};
	Unit ->
	    make_move(Tail, Game, Unit, {startpos, X, Y})
    end.

% Arguments: [Ending position|Rest of pos], Current game, Unit to move, Starting pos
% Checks if the positions are valid
% if they are, places the unit on the requested ending position, removes the unit from the start position and returns the updated game record
% else returns {error, Reason}
make_move([{position, EX, EY}| Tail], Game, Unit, Start) when length(Tail) =/= 0 ->
    NextPos = get_tile(Game#game.tilemap, EX, EY),
    case NextPos of
	{error, "Out of bounds"} ->
	    {error, "Out of bounds"};
	{ok, Tile} ->
	    if
		(Tile#tile.unit =/= null)-> % AND WATER...
		    {error, "Occupied tile in the way"};
		true ->
		    make_move(Tail, Game, Unit, Start)
	    end
    end;

make_move([{position, EX, EY}], Game, Unit, {startpos, SX, SY}) ->
    Unitmap = Game#game.tilemap,
    case remove_unit(Unitmap, SX, SY) of
	{ok, NewUnitmap} ->
	    case add_unit(NewUnitmap, Unit, EX, EY) of
		{ok, Map} -> 
		    io:format("Moved unit ~p from {~p,~p} to {~p,~p}~n", [Unit#unit.str, SX,SY, EX, EY]),
		    {ok, Game#game{tilemap = Map}};
		{error, Reason} -> 
		    {error, Reason}
	    end;
	{error, Reason} ->
	    {error, Reason}
    end.

insert_unit(UnitMap, {FX, FY}, {TX, TY}, Owner) ->
    FromUnit = get_unit(UnitMap, FX, FY),
    {ok, Endtile} = get_tile(UnitMap, TX, TY),

    if 
	(FromUnit =:= null) or (FromUnit =:= {error, "Out of bounds"}) or (Endtile =:= {error, "Out of bounds"}) ->
	    {error, "Invalid tile"};
	
	(Endtile#tile.city =/= null) ->
	    enter_city(UnitMap, {FX, FY}, {TX, TY});
	
	(Endtile#tile.unit =/= null) ->
	    EndUnit = Endtile#tile.unit,
	    case EndUnit#unit.name of
		trireme ->
		    enter_ship(UnitMap, {FX, FY}, {TX, TY});
		galley ->
		    enter_ship(UnitMap, {FX, FY}, {TX, TY});
		caravel ->
		    enter_ship(UnitMap, {FX, FY}, {TX, TY});
		siege_tower ->
		    enter_tower(UnitMap, {FX, FY}, {TX, TY});
		_ ->
		    {error, "Invalid tile"}
	    end
    end.
    
enter_city(UnitMap, {FX, FY}, {TX, TY}) ->
    OldCity = get_city(UnitMap, TX, TY),
    OldUnits = OldCity#city.units,
    EnteringUnit = get_unit(UnitMap, FX, FY),
    UpdatedCity = OldCity#city{units = OldUnits ++ [EnteringUnit]},
    {ok, UpdatedUnitMap1} = remove_unit(UnitMap, FX, FY),
    {ok, OldTile} = get_tile(UpdatedUnitMap1, TX, TY),
    UpdatedTile = OldTile#tile{city = UpdatedCity},
    UpdatedUnitMap2 = update_tile(UpdatedUnitMap1, UpdatedTile, TX, TY),
    {ok, UpdatedUnitMap2}.

enter_ship(UnitMap, {FX, FY}, {TX, TY}) ->
    ok.
enter_tower(UnitMap, {FX, FY}, {TX, TY}) ->
    enter_ship.


extract_unit(UnitMap, {CX, CY}, UnitType, MP, Owner, {TX, TY}) ->
    CTile = get_tile(UnitMap, CX, CY),
    ToPlace = get_unit(UnitMap, TX, TY),
    
    if
	(ToPlace =/= null) or (ToPlace =:= {error, "Out of bounds"}) ->
	    {error, "Invalid tile"};

	true -> % else

	    case CTile of
		{error, "Out of bounds"} ->
		    {error, "Out of bounds"};

		{ok, ContainerTile} ->
		    if
			(ContainerTile#tile.city =/= null) ->
			    leave_city(UnitMap, {CX, CY}, UnitType, MP, {TX, TY});

			(ContainerTile#tile.unit =/= null) ->
			  
			    ContainerType = ContainerTile#tile.unit,

			    case ContainerType#unit.name of
				trireme ->
				    ok;%leave_ship(UnitMap, {FX, FY}, {TX, TY});
				galley ->
				    ok;%leave_ship(UnitMap, {FX, FY}, {TX, TY});
				caravel ->
				    ok;%leave_ship(UnitMap, {FX, FY}, {TX, TY});
				siege_tower ->
				    ok;%leave_tower(UnitMap, {FX, FY}, {TX, TY});
				_ ->
				    {error, "Invalid tile"}
			    end
		    end
	    end
    end.
			
leave_city(UnitMap, {CX, CY}, UnitType, MP, {TX, TY}) ->
    OldCity = get_city(UnitMap, CX, CY),
    OldCityList = OldCity#city.units,
    
    Find_Unit = fun(UR) ->
	(UR#unit.str =:= UnitType) and (UR#unit.mp =:= MP)
    end,

    MatchingUnits = lists:filter(Find_Unit, OldCityList),

    case length(MatchingUnits) of
	0 ->
	    io:format("Unit not found"),
	    _TheUnit = {error, "Unit not found"};
	_ ->
	    io:format("Units found"),
	    TheUnit = hd(MatchingUnits), %hitta unit
	    UpdatedCityList = lists:delete(TheUnit, OldCityList), %ta bort från staden
	    UpdatedCity = OldCity#city{units = UpdatedCityList}, %uppdatera staden
	    OldCityTile = get_tile(UnitMap, CX, CY), 
	    UpdatedCityTile = OldCityTile#tile{city = UpdatedCity}, %skapa ny tile med uppdaterade staden på
	    UpdatedUnitMap1 = update_tile(UnitMap, UpdatedCityTile, CX, CY), %uppdatera tilen där staden stod med den nya tilen

	    OldUnitTile = get_tile(UpdatedUnitMap1, TX, TY), %hämta tilen där enheten ska placeras
	    UpdatedUnitTile = OldUnitTile#tile{unit = TheUnit}, % skapa ny tile med enheten på
	    UpdatedUnitMap2 = update_tile(UpdatedUnitMap1, UpdatedUnitTile, TX, TY), %uppdatera tilen där enheten ska stå med den nya tilen
	    {ok, UpdatedUnitMap2}
    end.


% Arguments: Unitmap, AttackPos, DefPos
% Attacks from Attackpos to Def pos,
% If an error occurs, such as Positions out of bounds, no units at positions or units out of range(soon to be implemented) - returns {error, Reason}
% Else - returns {ok, UpdatedUnitMap, {Remaining Attack Manpower, Remaining Defense Manpower}} (e.g {ok, UnitMap, {Int, Int}})
attack_unit(UnitMap, TerrainMap, {AttX, AttY}, {DefX, DefY}) -> %GLÖM EJ RANGEKOLL
    AttTerrain = lists:nth(AttY, lists:nth(AttX, TerrainMap)),
    DefTerrain = lists:nth(DefY, lists:nth(DefX, TerrainMap)),
    AttackUnit = get_unit(UnitMap, AttX, AttY),
    DefUnit = get_unit(UnitMap, DefX, DefY),
    {ok, AttackTile} = get_tile(UnitMap, AttX, AttY),
    {ok, DefTile} = get_tile(UnitMap, DefX, DefY),

    if 
	(AttackUnit =:= null) or (DefUnit =:= null) ->
	    {error, "Invalid tile"};
	(AttackUnit =:= {error, "Out of bounds"}) or (DefUnit =:= {error, "Out of bounds"}) ->
	    {error, "Out of bounds"};
	(AttackTile =:= {error, "Out of bounds"}) or (DefTile =:= {error, "Out of bounds"}) ->
	    {error, "Out of bounds"};
%get_range({X1,Y1},{X2,Y2})->
%	Z1 = X1 - Y1,		
%	Z2 = X2 - Y2,		
%	erlang:max(erlang:max(dif(X1,X2),dif(Y1,Y2)),dif(Z1,Z2)).
%dif(X1,X2)->
%	erlang:max(X1,X2) - erlang:min (X1,X2).


	true -> %else
	    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain),
	    io:format("~p with ~p manpower on ~p-terrain from {~p,~p} attacked ~p with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left", [AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, AttX, AttY, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]),
	    if
		(RemAttackMp =< 0) and (RemDefMp =< 0) ->
		    {ok, FirstUpdatedUnitMap} = remove_unit(UnitMap, AttX, AttY), 
		    {ok, SecondUpdatedUnitMap} = remove_unit(FirstUpdatedUnitMap, DefX, DefY),
		    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}};
		(RemAttackMp =< 0) ->
		    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
		    {ok, FirstUpdatedUnitMap} = remove_unit(UnitMap, AttX, AttY),
		    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdDefUnit, DefX, DefY), 
		    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}};
		(RemDefMp =< 0) ->
		    UpdAttackUnit = AttackUnit#unit{mp = RemAttackMp},
		    {ok, FirstUpdatedUnitMap} = remove_unit(UnitMap, DefX, DefY),
		    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdAttackUnit, AttX, AttY), 
		    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}};
		true -> %else
		    UpdAttackUnit = AttackUnit#unit{mp = RemAttackMp},
		    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
		    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdAttackUnit, AttX, AttY), 
		    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdDefUnit, DefX, DefY),
		    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}}
	    end
    end.	    



build_city(UnitMap, {X, Y}, CityName, CityOwner) ->
    Settler = get_unit(UnitMap, X, Y),
    {ok, Tile} = get_tile(UnitMap, X, Y), 
    ExistingCity = get_city(UnitMap, X, Y),
    
    if
	(Settler =:= {error, "Out of bounds"}) or (Settler#unit.name =/= settler) ->
	    {error, "Invalid tile"};
	(ExistingCity =:= {error, "Out of bounds"}) or (ExistingCity =/= null) ->
	    {error, "Invalid tile"};
	(Tile =:= {error, "Out of bounds"}) ->
	    {error, "Out of bounds"};
	
	true -> %else
	   City = #city{name = CityName, owner = CityOwner},
	   UpdatedTile = Tile#tile{city = City},
	   {ok, UpdatedUnitMap1} = remove_unit(UnitMap, X, Y),
	   UpdatedUnitMap2 = update_tile(UpdatedUnitMap1, UpdatedTile, X, Y), 
	   {ok, UpdatedUnitMap2}
   end.
