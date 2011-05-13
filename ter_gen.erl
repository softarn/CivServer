%% 	Random terrain generator
% 	Authors: 	Arian Jafari & Rasmus Ivö
%	aria-jaf@dsv.su.se
%	rasm-ivo@dsv.su.se
% =============================
% The method generate/2 takes two parameters; width and height, 
% and returns a matrix (e g lists containing lists).
% 
% Originally, the matrix contains integers ranging between 1 and 12,
% each representing a type of terrain. The matrix is then manipulated,
% with each element being replaced by a string with the name of the
% terrain type.
%
% A lot of the numbers are easily tweaked. By increasing/decreasing
% the WATER_FACTOR constant, you can change the weighted ratio of
% water to land. Change the LANDTYPE_FACTOR constant in order to 
% increase/decrease the influence neighbouring terrain types has when
% determining a position's terrain type.
%
% Ocean tiles are just generated when all neighbours is water-tiles.
% Note: the generate_ocean function removes islands (lone land tiles in water)
% as it is right now.
% 
% If you have any questions please feel free to contact either one of us.

-module(ter_gen).
-export([generate/2]).
-define(WATER_FACTOR, 6).		% Decrease for more water, and vice versa.
-define(LANDTYPE_FACTOR, 5).	% Decrease for higher terrain homogeneity, and vice versa.

%% Returns a matrix (a list containing lists) representing a randomly
%% generated terrain map, with the given width and height.
%% This is the only method you need to call.

generate(Width,Height) when is_number(Width), is_number(Height)->
	random:seed(now()),
	TheMap = generate_ocean(create_map(Width, Height)),
	% Water = check_map(Width, Height, TheMap, Width, Height, 0),
	% case Water > ______ of
	%	true ->
	%		create_map(Width, Height);
	%	false ->
	%		ok
	% end
	% generate_ocean(TheMap),	
	StringMap = stringify(TheMap, Width, Height, Width, Height),
	[tuple_to_list(Column)|| Column <- tuple_to_list(StringMap)].

create_map(Width,Height)->
	Matrix = erlang:make_tuple(Width,erlang:make_tuple(Height,1)),
	generate_tiles(Width, Height, Matrix, Width, Height).

%% Iterates through the matrix, procedurally 
%% choosing the terrain type for each position.
generate_tiles(0,_, Matrix,_, _) ->
	Matrix;
generate_tiles(Row, 1, Matrix, Width, Height) ->
	NewMatrix = choose_tile(Row, 1, Matrix),
	generate_tiles(Row-1, Height, NewMatrix, Width, Height);
generate_tiles(Row, Column, Matrix, Width, Height) ->
	NewMatrix = choose_tile(Row, Column, Matrix),
	generate_tiles(Row, Column-1, NewMatrix, Width, Height).

%% Sets the integer of a given position in the matrix.
change_terrain(Row,Column,Terrain,Matrix)->
	setelement(Row,Matrix,setelement(Column,element(Row,Matrix),Terrain)).	

%% Returns the integer of a given position in the matrix.
get_terrain(Row,Column,Matrix)->
	element(Column,element(Row,Matrix)).

%% Returns a list containing the coordinates
%% of a given tile's neighbours.
get_neighbours(_,_,List,[],_) ->
	List;
get_neighbours(Row,Column,_,_,Matrix) when Row<1; Row>tuple_size(Matrix); Column<1;Column>tuple_size(element(Row,Matrix))->
	{error,out_of_bounds};
get_neighbours(Row,Column,List,[{Y,X}|Tail],Matrix) ->	
	NeighY = Row + Y,
	case ((NeighY =< tuple_size(Matrix)) and (NeighY>0)) of  	
		false -> get_neighbours(Row,Column,List,Tail,Matrix);
		true ->
			NeighX = Column + X,
			case ((NeighX =< tuple_size(element(Row,Matrix))) and  (NeighX>0)) of  	
				false ->get_neighbours(Row,Column,List,Tail,Matrix);
				true -> get_neighbours(Row,Column,[{NeighY,NeighX}|List],Tail,Matrix)
			end
	end.

%% Takes a list of integers and counts the number of times a certain
%% integer (representing a terrain type) appears in the given list.
count_terrain([],_,_,Acc)->
	Acc;
count_terrain([{Y,X}|Tail],Matrix,Terrain,Acc)->
	case get_terrain(Y,X,Matrix) =:= Terrain of
		true ->count_terrain(Tail,Matrix,Terrain,Acc+1);
		_->	count_terrain(Tail,Matrix,Terrain,Acc)
	end. 	

%% Determines the type of land a certain tile will have through
%% a weighted random number, determined by adjusting to the adjacent terrain tiles.
det_type(Row,Column,Matrix,_,2,Terrain,_)->
		change_terrain(Row, Column, Terrain, Matrix);
det_type(Row,Column,Matrix,Neigh,Count,Terrain,Max)->
		TerrainCount = count_terrain(Neigh,Matrix,Count,0) + random:uniform(?LANDTYPE_FACTOR),
		case TerrainCount > Max of	
			true->det_type(Row,Column,Matrix,Neigh,Count-1,Count,TerrainCount);
			false->det_type(Row,Column,Matrix,Neigh,Count-1,Terrain,Max)
		end.

%% Method for choosing the terrain type for a given position in the matrix.
%% Gathers the terrain type of the tile's neighbours, and then makes
%% a random (yet weighted) decision based upon the neighbouring terrain types.
%% Returns the updated matrix with the performed changes.
choose_tile(Row,Column,Matrix)->
	Neigh = get_neighbours(Row,Column,[],neighbour_coords(),Matrix),	
	Water = count_terrain(Neigh,Matrix,1,0),
	Weight = Water - (length(Neigh) - Water),
	WaterOrLand = (random:uniform(?WATER_FACTOR) - 1) - Weight,
	case WaterOrLand < 0 of
		false ->
			det_type(Row,Column,Matrix,Neigh,12,3,0);
		true->
			Matrix
	end.

%% Goes through the matrix and changes Sea (1) tiles completely surrounded
%% by water into Ocean (2) tiles. Also eliminates one tile large islands,
%% e.g. land tiles surrounded by 6 water tiles.
generate_ocean(Matrix)->
	generate_ocean(Matrix,tuple_size(Matrix),tuple_size(element(1,Matrix))).
generate_ocean(Matrix,1,1)->
	type_of_water(1,1,Matrix,get_neighbours(1,1,[],neighbour_coords(),Matrix));	
generate_ocean(Matrix,Row,1)->
	generate_ocean(type_of_water(Row,1,Matrix,get_neighbours(1,1,[],neighbour_coords(),Matrix)),Row-1,tuple_size(element(Row-1,Matrix)));
generate_ocean(Matrix,Row,Column)->
	generate_ocean(type_of_water(Row,Column,Matrix,get_neighbours(Row,Column,[],neighbour_coords(),Matrix)),Row,Column-1).

type_of_water(Row,Column,Matrix, [])->
	change_terrain(Row, Column, 2, Matrix);	
type_of_water(Row,Column,Matrix, [{Y,X}|Tail])->
	case get_terrain(Y,X,Matrix) < 3 of
			true -> type_of_water(Row,Column,Matrix,Tail);
			false -> Matrix	end.

%% -----STUB-----
%% Counts the number of water tiles the matrix contains.
%% To be used for checking whether a map contains a sufficient
%% ratio of water:land or not, possibly re-generating the map
%% if not satisfactory.
%check_map(1, 1, _, _, _, Acc) ->
%	Acc;
%check_map(Row, 1, Matrix, Width, Height, Acc) ->
%	check_map(Row-1, Height, Matrix, Width, Height, Acc);
%check_map(Row, Column, Matrix, Width, Height, Acc) ->
%	case get_terrain(Row, Column, Matrix) of
%		1 ->
%			NewAcc = Acc+1;
%		_ ->
%			NewAcc = Acc
%	end,
%	check_map(Row, Column-1, Matrix, Width, Height, NewAcc).

%% Converts the integer matrix to a String matrix containing
%% names of the different terrain types.
stringify(Matrix, 1, 0, _, _) ->
	Matrix;
stringify(Matrix, Row, 0, Width, Height) ->
	stringify(Matrix, Row-1, Height, Width, Height);
stringify(Matrix, Row, Column, Width, Height) ->
	TerInt = get_terrain(Row, Column, Matrix),
	TerStr = element(TerInt, terrain_names()),
	NewMatrix = change_terrain(Row, Column, TerStr, Matrix),
	stringify(NewMatrix, Row, Column-1, Width, Height).
	
	
%% Coordinates for finding a certain tile's neighbours.
neighbour_coords()->
	[{1,0},{-1,0},{0,1},{0,-1},{1,1},{-1,-1}].
	
%% Used as a map-like data structure for swapping out integers to terrain names.
terrain_names() ->
	{"Sea", "Ocean", "Plains", "Grassland", "Marsh", "Desert",
	"Tundra", "Rain Forrest", "Conifer Forrest", "Broadleaf Forrest",
	"Hills", "Mountains"}.
