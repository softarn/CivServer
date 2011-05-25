-module(gameplan).
-include("config.hrl").
-compile(export_all).


% Arguments: Size of maps to be genererated, The current Game
% Generates a Terrain-map,
% Generates a empty Unit-map (tuples) with tilerecords containing their position
% Adds starting units to the Unit-map
% Updates the game-record with the new maps and returns the updated game record
make_gameplan(Width, Height, Game) ->
    Players = Game#game.players,
    TerrainMatrix = ?TERGEN:generate(Width, Height),
    UnitMatrix = erlang:make_tuple(Width, erlang:make_tuple(Height, #tile{})),

    UpdUnitMatrix = add_pos(UnitMatrix, Width, Height, Height),
    NumberOfPlayers = length(Game#game.players),

    StartPos = ?START_POS:get_placement(NumberOfPlayers, TerrainMatrix),
    UpdatedUnitMatrix = assign_pos(Players, StartPos, UpdUnitMatrix),
    Game#game{map = TerrainMatrix, tilemap = UpdatedUnitMatrix}. 

% Arguments: List of Players to be assinged a starting position, List of positions {X, Y}, The Unit-map to be updated with units at starting positions
% Adds a unit (currently a Knight) on the positions, one unit for each player
% Returns the updated unit-map
assign_pos([], [], UMap) ->
    UMap;
assign_pos([Player|PlayerRest], [Pos1, Pos2|PosRest], UMap) ->
    {ok, UpdatedUMap} = create_unit(UMap, Pos1, "Settler", Player#player.name),
    {ok, UpdatedUMap2} = create_unit(UpdatedUMap, Pos2, "Settler", Player#player.name),
    assign_pos(PlayerRest, PosRest, UpdatedUMap2).

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
			    UpdatedMap = update_tile(Map, NewTile, X, Y),
			    case get_city(Map, X, Y) of
				null ->
				    {ok, UpdatedMap};
				_ ->
				    insert_unit(UpdatedMap, {X,Y},{X,Y})
			    end;		
			{error, Reason} ->
			    {error, Reason}
		    end
	    end;
	_ ->
	    {error, "Invalid tile"}    
    end.
% Arguments: The map in which to remove a unit, Pos of the tile
% Removes a unit from the tile at the given position
%% Returns {ok, updated unit map}
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

    case get_tile(Unitmap, EX, EY) of
	{ok, Tile} ->
	    io:format("~p~n", [Tile]),
	    if
		(Tile#tile.city =/= null) ->
		    City = Tile#tile.city,
		    case same_owner(Unit, City) of
			true ->
			    {ok, UpdatedUnitMap} = insert_unit(Unitmap, {SX, SY}, {EX, EY}),
			    {ok, Game#game{tilemap = UpdatedUnitMap}};
			false ->
			    case is_empty(City) of
				true ->
				    io:format("~p took over ~p's city ~p~n", [Unit#unit.owner, City#city.owner, City#city.name]),
				    NewCity = City#city{owner = Unit#unit.owner},
				    UpdatedTile = Tile#tile{city = NewCity},
				    UpdatedUnitMap1 = update_tile(Unitmap, UpdatedTile, EX, EY),
				    {ok, UpdatedUnitMap2} = insert_unit(UpdatedUnitMap1, {SX, SY}, {EX, EY}),
				    {ok, Game#game{tilemap = UpdatedUnitMap2}};
				false ->
				    {error, "Invalid, full of enemies"}
			    end
		    end;

		((Tile#tile.unit)#unit.name =:= trireme) ->
		    {ok, UpdatedUnitMap} = insert_unit(Unitmap, {SX, SY}, {EX, EY}),
		    {ok, Game#game{tilemap = UpdatedUnitMap}};

		((Tile#tile.unit)#unit.name =:= galley) ->
		    {ok, UpdatedUnitMap} = insert_unit(Unitmap, {SX, SY}, {EX, EY}),
		    {ok, Game#game{tilemap = UpdatedUnitMap}};

		((Tile#tile.unit)#unit.name =:= siege_tower) ->
		    Tower = Tile#tile.unit,
		    case same_owner(Unit, Tower) of
			true ->
			    {ok, UpdatedUnitMap} = insert_unit(Unitmap, {SX, SY}, {EX, EY}),
			    {ok, Game#game{tilemap = UpdatedUnitMap}};
			false ->
			    case is_empty(Tower) of
				true ->
				    io:format("~p took over ~p's siegetower~n", [Unit#unit.owner, Tower#unit.owner]),
				    NewTower = Tower#unit{owner = Unit#unit.owner},
				    UpdatedTile = Tile#tile{unit = NewTower},
				    UpdatedUnitMap1 = update_tile(Unitmap, UpdatedTile, EX, EY),
				    io:format("Hej ~p~n", [NewTower]),
				    {ok, UpdatedUnitMap2} = insert_unit(UpdatedUnitMap1, {SX, SY}, {EX, EY}),
				    {ok, Game#game{tilemap = UpdatedUnitMap2}};
				false ->
				    {error, "Invalid, full of enemies"}
			    end
		    end;

		((Tile#tile.unit)#unit.name =:= caravel) ->
		    {ok, UpdatedUnitMap} = insert_unit(Unitmap, {SX, SY}, {EX, EY}),
		    {ok, Game#game{tilemap = UpdatedUnitMap}};

		true ->
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
		    end
	    end;

	{error, Reason} ->
	    {error, Reason}
    end.

%Arguments: Unitmap, FromX, FromY, ToX, ToY, Owner of unit to be inserted
% Returns error if:
% -there is no unit at FX, FY
% -FX, FY or TX,TY isnt on the map
% -If the unit at TX,TY isnt a "containerunit" or a city
%Else Checks:
%if TX,TY contains a City, and then calls enter city (Returns {ok, UpdatedUnitMap}
%if TX,TY contains a "containerunit" of type trireme, galley, caravel or siegetower, and then calls enter_Unittype (returns {ok, UpdatedUnitMap}
insert_unit(UnitMap, {FX, FY}, {TX, TY}) ->
    FromUnit = get_unit(UnitMap, FX, FY),
    {ok, Endtile} = get_tile(UnitMap, TX, TY),

    if 
	(FromUnit =:= null) or (FromUnit =:= {error, "Out of bounds"}) or (Endtile =:= {error, "Out of bounds"}) ->
	    {error, "Invalid tile"};

	(Endtile#tile.city =/= null) ->
	    case same_owner(Endtile#tile.city, FromUnit) of
		true ->
		    {ok, NewUnitMap} = remove_unit(UnitMap, FX, FY),
		    enter_container(NewUnitMap, FromUnit, {TX, TY}, city);
		false ->
		    {error, "Permission denied"}
	    end;

	(Endtile#tile.unit =/= null) ->
	    EndUnit = Endtile#tile.unit,
	    case {is_container_unit(EndUnit),
		    same_owner(EndUnit, FromUnit)} of
		{true, true} ->
		    {ok, NewUnitMap} = remove_unit(UnitMap, FX, FY),
		    enter_container(NewUnitMap, FromUnit, {TX, TY}, unit);
		_ ->
		    {error, "Invalid tile"}
	    end;

	true ->
	    {error, "Invalid tile"}
    end.

%Arguments: UnitMap, FromX, FromY, ToX, ToY
% If the unitowner is not the cityowner - returns {error, "Permission denied"} 
%Else
% Gets the city at TX,To and adds the unit at FX, FY to the city's unitlist,
% Removes the unit from FX, FY and updates the tile at TX, TY with the updated city
%Returns {ok, UpdatedUnitMap}
enter_container(UnitMap, EnteringUnit, {TX, TY}, Type) ->
    {ok, OldTile} = get_tile(UnitMap, TX, TY),

    case Type of
	city ->
	    OldContainer = get_city(UnitMap, TX, TY),
	    OldUnits = OldContainer#city.units,
	    UpdatedContainer = OldContainer#city{units = OldUnits ++ [EnteringUnit]},
	    UpdatedTile = OldTile#tile{city = UpdatedContainer},
	    io:format("~p entered the city ~p ~n", [EnteringUnit#unit.str, UpdatedContainer#city.name]);

	unit ->
	    OldContainer = get_unit(UnitMap, TX, TY),
	    OldUnits = OldContainer#unit.units,
	    UpdatedContainer = OldContainer#unit{units = OldUnits ++ [EnteringUnit]},
	    UpdatedTile = OldTile#tile{unit = UpdatedContainer},
	    io:format("~p entered the unit ~p ~n", [EnteringUnit#unit.str, UpdatedContainer#unit.name])
    end,
    NewUnitMap = update_tile(UnitMap, UpdatedTile, TX, TY),
    {ok, NewUnitMap}.

%Arguments: Unitmap, ContainerX, ContainerY, Unittype, Manpower, ToX, ToY,
% Returns {eror, Reason} if:
% -the ToX,ToY-tile is occupied or out of bounds
% -the Containertile is out of bounds, not a "containerunit" nor a city
% else calls on leave_ContainerType method and returns {ok, UpdatedUnitMap}
extract_unit(UnitMap, {CX, CY}, UnitType, MP, {TX, TY}) ->
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
			    leave_container(UnitMap, {CX, CY}, UnitType, MP, {TX, TY}, city);

			(ContainerTile#tile.unit =/= null) ->
			    case is_container_unit(ContainerTile#tile.unit) of
				true ->
				    leave_container(UnitMap, {CX, CY}, UnitType, MP, {TX, TY}, unit);
				false ->
				    {error, "Invalid tile"}
			    end;

			true ->
			    {error, "Invalid tile"}

		    end
	    end
    end.

%Arguments: UnitMap, CityX,CityY, UnitType, manpower, ToX, ToY
%%filters the buildinglist and checks the length.
% if the length is:
% 0 ->
% returns {error, "Unit not found"}

% Anything else ->
% Gets the head of the list (of identical units) which is the unit that wants to exit
% Deletes the unit from the citylist and updates the tile with the new city,
% Gets the tile where to place the exiting unit and places it there and updates the unitmap with the tile
% returns {ok, UpdatedUnitMap}
leave_container(UnitMap, {CX, CY}, UnitType, MP, {TX, TY}, Type) ->
    case Type of
	city ->
	    OldContainer = get_city(UnitMap, CX, CY),
	    OldList = OldContainer#city.units;
	unit ->
	    OldContainer = get_unit(UnitMap, CX, CY),
	    OldList = OldContainer#unit.units
    end,

    Find_Unit = fun(UR) ->
	    (UR#unit.str =:= UnitType) and (UR#unit.mp =:= MP)
    end,

    MatchingUnits = lists:filter(Find_Unit, OldList),

    case length(MatchingUnits) of
	0 ->
	    io:format("Unit not found in container~n"),
	    {error, "Unit not found"};
	_ ->
	    Unit = hd(MatchingUnits), %hitta unit
	    UpdatedList = lists:delete(Unit, OldList), %ta bort från containern 
	    {ok, OldTile} = get_tile(UnitMap, CX, CY), 

	    if 
		(Type =:= city) ->
		    UpdatedContainer = OldContainer#city{units = UpdatedList}, %uppdatera containern 
		    UpdatedTile = OldTile#tile{city = UpdatedContainer}; %skapa ny tile med uppdaterade staden på
		(Type =:= unit) ->
		    UpdatedContainer = OldContainer#unit{units = UpdatedList}, %uppdatera containern 
		    UpdatedTile = OldTile#tile{unit = UpdatedContainer} %skapa ny tile med uppdaterade staden på
	    end,
	    UpdatedUnitMap1 = update_tile(UnitMap, UpdatedTile, CX, CY), %uppdatera tilen där staden stod med den nya tilen

	    {ok, ToTile} = get_tile(UpdatedUnitMap1, TX, TY),
	    case get_container(ToTile) of
		unit -> {ok, UpdatedUnitMap2} = enter_container(UpdatedUnitMap1, Unit, {TX, TY}, unit);
		city -> {ok, UpdatedUnitMap2} = enter_container(UpdatedUnitMap1, Unit, {TX, TY}, city);
		none -> 
		    {ok, OldToTile} = get_tile(UpdatedUnitMap1, TX, TY), %hämta tilen där enheten ska placeras
		    UpdatedToTile = OldToTile#tile{unit = Unit}, % skapa ny tile med enheten på
		    UpdatedUnitMap2 = update_tile(UpdatedUnitMap1, UpdatedToTile, TX, TY) %uppdatera tilen där enheten ska stå med den nya tilen
	    end,
	    io:format("~p left a city/ship/tower~n", [UnitType]),
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
    DefCity = get_city(UnitMap, DefX, DefY),
    if 
	((DefCity=:=null) or (DefCity=:={error, "Out of bounds"})) ->
	    DefUnit = get_unit(UnitMap, DefX, DefY);
	true ->
	    DefUnit = get_city_def(DefCity#city.units)
    end,	
    {ok, AttackTile} = get_tile(UnitMap, AttX, AttY),
    {ok, DefTile} = get_tile(UnitMap, DefX, DefY),
    AttInsideUnit = get_siege_unit(AttackUnit),
    DefInsideUnit = get_siege_unit(DefUnit),

    if 
	(AttackUnit =:= null) or (DefUnit =:= null) ->
	    {error, "Invalid tile"};
	(AttackUnit =:= {error, "Out of bounds"}) or (DefUnit =:= {error, "Out of bounds"}) ->
	    {error, "Out of bounds"};
	(AttackUnit#unit.name =:= siege_tower) and (AttInsideUnit =:= null) ->
	    {error, "Empty siegetower"};
	(DefUnit#unit.name =:= siege_tower) and (DefInsideUnit =:= null) ->
	    {error, "Empty siegetower"};
	(AttackTile =:= {error, "Out of bounds"}) or (DefTile =:= {error, "Out of bounds"}) ->
	    {error, "Out of bounds"};

	true ->
	    VictimStr = DefUnit#unit.owner,

	    BombBool = lists:member(AttackUnit#unit.name, [catapult, trebuchet, cannon, galley, caravel]),

	    if 
		(BombBool =:= true) and (DefTile#tile.city =/= null) -> %City bombardment
		    DefCity = DefTile#tile.city,
		    DefUnits = DefCity#city.units,
		    UnitMp = [X#unit.mp || X <- DefUnits],
		    DPU = ?BOMB:bombard(AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, UnitMp),

		    UpdateUnitFun = fun(UR) ->
			    UpdUR = UR#unit{mp = UR#unit.mp - DPU},
			    UpdUR
		    end,

		    AboveZero = fun(UR) ->
			    UR#unit.mp >= 0
		    end,

		    UpdatedUnits1 = [UpdateUnitFun(X) || X <- DefUnits],
		    UpdatedUnits2 = lists:filter(AboveZero, UpdatedUnits1),
		    UpdatedCity = DefCity#city{units = UpdatedUnits2},
		    UpdatedDefTile = DefTile#tile{city = UpdatedCity},
		    UpdatedUnitMap = update_tile(UnitMap, UpdatedDefTile, DefX, DefY), 
		    io:format("~p bombarded ~p's city ~p. Result: Defender lost ~p mp~n", [AttackUnit#unit.owner, DefCity#city.owner, DefCity#city.name, DPU]),
		    {bombardment, UpdatedUnitMap, {DefX, DefY}, DPU, VictimStr};

		true -> %else
		    case DefCity of
			null ->
			    City_combat = false;
			_ ->
			    City_combat = true
		    end,

		    if

			((AttackUnit#unit.name =:= siege_tower) and (DefUnit#unit.fortified =:= true)) ->
			    AttUnit = get_siege_unit(AttackUnit),
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttUnit, AttTerrain, DefUnit, DefTerrain, {City_combat, true}),
			    DefMpLost = DefUnit#unit.mp - RemDefMp,
			    io:format("~p in a siegetower with ~p manpower on ~p-terrain from {~p,~p} attacked a fortified ~p with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttUnit#unit.str, AttUnit#unit.mp, AttTerrain, AttX, AttY, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]);

			((AttackUnit#unit.name =:= siege_tower) and (DefUnit#unit.name =:= siege_tower)) ->
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttInsideUnit, AttTerrain, DefInsideUnit, DefTerrain, {City_combat, false}),
			    DefMpLost = DefInsideUnit#unit.mp - RemDefMp,
			    io:format("~p in a siegetower with ~p manpower on ~p-terrain from {~p,~p} attacked a ~p in a siegetower with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttInsideUnit#unit.str, AttInsideUnit#unit.mp, AttTerrain, AttX, AttY, DefInsideUnit#unit.str, DefInsideUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]);
			
			(AttackUnit#unit.name =:= siege_tower) ->
			    AttUnit = get_siege_unit(AttackUnit),
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttUnit, AttTerrain, DefUnit, DefTerrain, {City_combat, false}),
			    DefMpLost = DefUnit#unit.mp - RemDefMp,
			    io:format("~p in a siegetower with ~p manpower on ~p-terrain from {~p,~p} attacked ~p with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttUnit#unit.str, AttUnit#unit.mp, AttTerrain, AttX, AttY, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]);

			(DefUnit#unit.fortified =:= true) ->
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttackUnit, AttTerrain, DefUnit, DefTerrain, {false, true}),
			    DefMpLost = DefUnit#unit.mp - RemDefMp,
			    io:format("~p  with ~p manpower on ~p-terrain from {~p,~p} attacked a fortified ~p with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, AttX, AttY, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]);

			(DefUnit#unit.name =:= siege_tower) ->
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttackUnit, AttTerrain, DefInsideUnit, DefTerrain, {City_combat, false}),
			    DefMpLost = DefInsideUnit#unit.mp - RemDefMp,
			    io:format("~p with ~p manpower on ~p-terrain from {~p,~p} attacked ~p in a siegetower with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, AttX, AttY, DefInsideUnit#unit.str, DefInsideUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp]);
			true -> %else
			    {RemAttackMp, RemDefMp} = ?COMBAT:combat(AttackUnit, AttTerrain, DefUnit, DefTerrain, {false, false}),
			    DefMpLost = DefUnit#unit.mp - RemDefMp,
			    io:format("~p with ~p manpower on ~p-terrain from {~p,~p} attacked ~p with ~p manpower on ~p-terrain on {~p,~p}. Result Attacker: ~p mp left, Defender: ~p mp left~n", [AttackUnit#unit.str, AttackUnit#unit.mp, AttTerrain, AttX, AttY, DefUnit#unit.str, DefUnit#unit.mp, DefTerrain, DefX, DefY, RemAttackMp, RemDefMp])
		    end,

		    FindUnit = fun(UR) ->
			    UR =/= DefUnit
		    end,	


		    if
			(RemAttackMp =< 0) and (RemDefMp =< 0) ->
			    case AttackUnit#unit.name =:= siege_tower of
				false ->
				    {ok, FirstUpdatedUnitMap} = remove_unit(UnitMap, AttX, AttY);
				true ->
				    UpdSiegeTower = AttackUnit#unit{units = []},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdSiegeTower, AttX, AttY)
			    end,

			    if 
				(City_combat =:= true) ->
				    UpdatedCityUnits = lists:delete(DefUnit, DefCity#city.units),
				    UpdatedCity = DefCity#city{units = UpdatedCityUnits},
				    {ok, OldTile} = get_tile(FirstUpdatedUnitMap, DefX, DefY),
				    UpdatedTile = OldTile#tile{city = UpdatedCity},
				    SecondUpdatedUnitMap = update_tile(FirstUpdatedUnitMap, UpdatedTile, DefX, DefY),
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}};
				true ->
				    case DefUnit#unit.name =:= siege_tower of
					false ->
					    {ok, SecondUpdatedUnitMap} = remove_unit(FirstUpdatedUnitMap, DefX, DefY);
					true ->
					    UpdSiegeTower2 = DefUnit#unit{units = []},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdSiegeTower2, DefX, DefY)
				    end,
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}}
			    end;

			(RemAttackMp =< 0) ->
			    case AttackUnit#unit.name =:= siege_tower of
				false ->
				    {ok, FirstUpdatedUnitMap} = remove_unit(UnitMap, AttX, AttY);
				true ->
				    UpdSiegeTower = AttackUnit#unit{units = []},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdSiegeTower, AttX, AttY)
			    end,

			    if
				(City_combat =:= true) ->
				    OldCityUnits = lists:filter(FindUnit, DefCity#city.units),
				    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
				    UpdatedCityUnits = [UpdDefUnit | OldCityUnits],
				    UpdatedCity = DefCity#city{units = UpdatedCityUnits},
				    {ok, OldTile} = get_tile(FirstUpdatedUnitMap, DefX, DefY),
				    UpdatedTile = OldTile#tile{city = UpdatedCity},
				    SecondUpdatedUnitMap = update_tile(FirstUpdatedUnitMap, UpdatedTile, DefX, DefY),
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}};
				true ->
				    case DefUnit#unit.name =:= siege_tower of
					false ->
					    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdDefUnit, DefX, DefY);
					true ->
					    DUnit = get_siege_unit(DefUnit),
					    UpdDefUnit = DUnit#unit{mp = RemAttackMp},
					    UpdSiegeTower2 = DefUnit#unit{units = [UpdDefUnit]},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdSiegeTower2, DefX, DefY)
				    end,
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}}
			    end;

			(RemDefMp =< 0) ->
			    case AttackUnit#unit.name =:= siege_tower of
				false ->
				    UpdAttackUnit = AttackUnit#unit{mp = RemAttackMp},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdAttackUnit, AttX, AttY);
				true ->
				    AUnit = get_siege_unit(AttackUnit),
				    UpdAttackUnit = AUnit#unit{mp = RemAttackMp},
				    UpdSiegeTower = AttackUnit#unit{units = [UpdAttackUnit]},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdSiegeTower, AttX, AttY)
			    end,

			    if
				(City_combat =:= true) ->
				    UpdatedCityUnits = lists:delete(DefUnit, DefCity#city.units),
				    UpdatedCity = DefCity#city{units = UpdatedCityUnits},
				    {ok, OldTile} = get_tile(FirstUpdatedUnitMap, DefX, DefY),
				    UpdatedTile = OldTile#tile{city = UpdatedCity},
				    SecondUpdatedUnitMap = update_tile(FirstUpdatedUnitMap, UpdatedTile, DefX, DefY),
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}};
				true ->
				    case DefUnit#unit.name =:= siege_tower of
					false ->
					    {ok, SecondUpdatedUnitMap} = remove_unit(FirstUpdatedUnitMap, DefX, DefY);
					true ->
					    UpdSiegeTower2 = DefUnit#unit{units = []},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdSiegeTower2, DefX, DefY)
				    end,
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}}
			    end;
			true -> %else
			    case AttackUnit#unit.name =:= siege_tower of
				false ->
				    UpdAttackUnit = AttackUnit#unit{mp = RemAttackMp},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdAttackUnit, AttX, AttY);
				true ->
				    AUnit = get_siege_unit(AttackUnit),
				    UpdAttackUnit = AUnit#unit{mp = RemAttackMp},
				    UpdSiegeTower = AttackUnit#unit{units = [UpdAttackUnit]},
				    {ok, FirstUpdatedUnitMap} = update_unit(UnitMap, UpdSiegeTower, AttX, AttY)
			    end,

			    if
				(City_combat =:= true) ->
				    OldCityUnits = lists:filter(FindUnit, DefCity#city.units),
				    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
				    UpdatedCityUnits = [UpdDefUnit | OldCityUnits],
				    UpdatedCity = DefCity#city{units = UpdatedCityUnits},
				    {ok, OldTile} = get_tile(FirstUpdatedUnitMap, DefX, DefY),
				    UpdatedTile = OldTile#tile{city = UpdatedCity},
				    SecondUpdatedUnitMap = update_tile(FirstUpdatedUnitMap, UpdatedTile, DefX, DefY),
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}};
				true ->
				    case DefUnit#unit.name =:= siege_tower of
					false ->
					    UpdDefUnit = DefUnit#unit{mp = RemDefMp},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdDefUnit, DefX, DefY);
					true ->
					    DUnit = get_siege_unit(DefUnit),
					    UpdDefUnit = DUnit#unit{mp = RemAttackMp},
					    UpdSiegeTower2 = DefUnit#unit{units = [UpdDefUnit]},
					    {ok, SecondUpdatedUnitMap} = update_unit(FirstUpdatedUnitMap, UpdSiegeTower2, DefX, DefY)
				    end,
				    {ok, SecondUpdatedUnitMap, {RemAttackMp, RemDefMp}, VictimStr, {DefMpLost, DefX, DefY}}
			    end
		    end
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
	    UpdatedTile = Tile#tile{city = City, unit = null},
	    UpdatedUnitMap1 = update_tile(UnitMap, UpdatedTile, X, Y), 
	    io:format("~p built a city named ~p at {~p, ~p}~n", [CityOwner, CityName, X, Y]),
	    {ok, UpdatedUnitMap1}
    end.

disband_unit(UnitMap, {X, Y}, Owner) ->
    Unit = get_unit(UnitMap, X, Y),

    case Unit of
	{error, "Out of bounds"} ->
	    {error, "Out of bounds"};
	null ->
	    {error, "Invalid tile"};
	_ ->
	    if
		(Unit#unit.owner =/= Owner) ->
		    {error, "Permission denied"};
		true ->
		    io:format("~p disbanded a ~p unit~n", [Owner, Unit#unit.str]),
		    {ok, UpdatedUnitMap} = remove_unit(UnitMap, X, Y),
		    {ok, UpdatedUnitMap}
	    end
    end.

same_owner(#city{owner = CityOwn}, #unit{owner = UnitOwn}) ->
    CityOwn == UnitOwn;
same_owner(#unit{owner = UnitOwn1}, #unit{owner = UnitOwn2}) ->
    UnitOwn1 == UnitOwn2;
same_owner(#unit{owner = UnitOwn}, #city{owner = CityOwn}) ->
    CityOwn == UnitOwn.

fortify_unit(UnitMap, {X, Y}, Owner) ->
    Unit = get_unit(UnitMap, X, Y),

    case Unit of
	{error, "Out of bounds"} ->
	    {error, "Out of bounds"};
	null ->
	    {error, "Invalid tile"};
	_ ->
	    if
		(Unit#unit.owner =/= Owner) ->
		    {error, "Permission denied"};
		true ->
		    FortUnit = Unit#unit{fortified = true},
		    io:format("~p fortified a ~p unit~n", [Owner, Unit#unit.str]),
		    update_unit(UnitMap, FortUnit, X, Y) %Returns {ok, UpdatedUnitMap}
	    end
    end.

unfortify_unit(UnitMap, {X, Y}, Owner) ->
    Unit = get_unit(UnitMap, X, Y),

    case Unit of
	{error, "Out of bounds"} ->
	    {error, "Out of bounds"};
	null ->
	    {error, "Invalid tile"};
	_ ->
	    if
		(Unit#unit.owner =/= Owner) ->
		    {error, "Permission denied"};
		true ->
		    UnFortUnit = Unit#unit{fortified = false},
		    io:format("~p un-fortified a ~p unit~n", [Owner, Unit#unit.str]),
		    update_unit(UnitMap, UnFortUnit, X, Y) %Returns {ok, UpdatedUnitMap}
	    end
    end.

is_container_unit(#unit{name = Name}) ->
    case Name of
	trireme -> true;
	galley -> true;
	caravel -> true;
	siege_tower -> true;
	_ -> false
    end.

get_container(#tile{city = City, unit = Unit}) when City =:= null ->
    if 
	(Unit =:= null) -> none;
	true ->
	    case Unit#unit.name of
		trireme -> unit;
		galley -> unit;
		caravel -> unit;
		siege_tower -> unit;
		_ -> none
	    end
    end;
get_container(#tile{city = City}) when City =/= null -> city.


% Fetches the best defender from a list of Unit records.

get_city_def([])->
    null;
get_city_def([Defender | Tail]) -> 
    get_city_def(Tail, Defender).

get_city_def([],Best_Defender)->
    Best_Defender;
get_city_def(Defenders,Best_defender)->
    %Contender to be best Defender!
    Contender = hd(Defenders),
    Contender_stats = unit_attr:get_attr(Contender#unit.name),
    Contender_def_power = round(element(3,Contender_stats)*(Contender#unit.mp)/100),
    %Reigning champion!
    Best_def_stats = unit_attr:get_attr(Best_defender#unit.name),
    Best_def_power = round(element(3,Best_def_stats)*(Best_defender#unit.mp)/100),

    % The winner takes it all!
    case Contender_def_power > Best_def_power of
	true ->
	    get_city_def(tl(Defenders), Contender);
	false ->
	    get_city_def(tl(Defenders),Best_defender)

    end.


is_empty(#city{units = CityUnits}) ->
    case CityUnits of
	[] -> true;
	_ -> false
    end;
is_empty(#unit{units = TowerUnits}) ->
    case TowerUnits of
	[] -> true;
	_ -> false
    end.

get_siege_unit(null) -> null;
get_siege_unit(UR) ->
    case UR#unit.units of
	[] ->
	    null;
	[H|_] ->
	    H
    end.
