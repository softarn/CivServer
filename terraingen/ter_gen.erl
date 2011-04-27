-module(ter_gen).
-export([generate/2]).


generate(Width,Height) when is_number(Width), is_number(Height)->
	TheMap = create_map(Width, Height),
	Water = check_map(Width, Height, TheMap, Width, Height, 0),
	%case Water > ______ of
	%	true ->
	%		create_map(Width, Height);
	%	false ->
	%		ok
	%end
	
	[tuple_to_list(Column)|| Column <- tuple_to_list(TheMap)].

create_map(Width,Height)->
	Matrix = erlang:make_tuple(Width,erlang:make_tuple(Height,1)),
	generate_tiles(Width, Height, Matrix, Width, Height).
	%spawn(?MODULE,loop,[erlang:make_tuple(Width,erlang:make_tuple(Height,1))]).
	%"Gandalf, there's fire below us!"
	%loop(erlang:make_tuple(Width,erlang:make_tuple(Height,1))).

generate_tiles(0,_, Matrix,_, _) ->
	Matrix;

generate_tiles(Row, 1, Matrix, Width, Height) ->
	NewMatrix = choose_tile(Row, 1, Matrix),
	generate_tiles(Row-1, Height, NewMatrix, Width, Height);

generate_tiles(Row, Column, Matrix, Width, Height) ->
	NewMatrix = choose_tile(Row, Column, Matrix),
	generate_tiles(Row, Column-1, NewMatrix, Width, Height).

change_terrain(Row,Column,Terrain,Matrix)->
	setelement(Row,Matrix,setelement(Column,element(Row,Matrix),Terrain)).	
get_terrain(Row,Column,Matrix)->
	element(Column,element(Row,Matrix)).

build_directions()->
	[{1,0},{-1,0},{0,1},{0,-1},{1,1},{-1,-1}].


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
count_terrain([],_,_,Acc)->
	Acc;
count_terrain([{Y,X}|Tail],Matrix,Terrain,Acc)->
	case get_terrain(Y,X,Matrix) =:= Terrain of
		true ->count_terrain(Tail,Matrix,Terrain,Acc+1);
		_->	count_terrain(Tail,Matrix,Terrain,Acc)
	end. 	

det_type(Row,Column,Matrix,_,1,Terrain,_)->
		change_terrain(Row, Column, Terrain, Matrix);
det_type(Row,Column,Matrix,Neigh,Count,Terrain,Max)->
		TerrainCount = count_terrain(Neigh,Matrix,Count,0) + random:uniform(5),
		case TerrainCount > Max of	
			true->det_type(Row,Column,Matrix,Neigh,Count-1,Count,TerrainCount);
			false->det_type(Row,Column,Matrix,Neigh,Count-1,Terrain,Max)
		end.

choose_tile(Row,Column,Matrix)->
	Neigh = get_neighbours(Row,Column,[],build_directions(),Matrix),	
	Water = count_terrain(Neigh,Matrix,1,0),
	Weight = Water - (length(Neigh) - Water),
	WaterOrLand = (random:uniform(6) - 1) - Weight,
	case WaterOrLand < 0 of
		false ->
			det_type(Row,Column,Matrix,Neigh,10,2,0);
		true->
			Matrix
	end.

check_map(1, 1, _, _, _, Acc) ->
	Acc;
check_map(Row, 1, Matrix, Width, Height, Acc) ->
	check_map(Row-1, Height, Matrix, Width, Height, Acc);
check_map(Row, Column, Matrix, Width, Height, Acc) ->
	case get_terrain(Row, Column, Matrix) of
		1 ->
			NewAcc = Acc+1;
		_ ->
			NewAcc = Acc
	end
	check_map(Row, Column-1, Matrix, Width, Height, NewAcc).
	
