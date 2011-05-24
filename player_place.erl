-module(player_place).
-compile(export_all).

get_placement(Players,Map) when is_list(Map) ->
    %Width = length(Map)
    get_base_pos(Players*2,list_to_tuple([list_to_tuple(X) || X<- Map])).

get_base_pos(Players,Map)->
    Width = size(element(1,Map)),
    Height = size(Map),	
    base_placement(Players,Map,{Width, Height}, dict:new()).

base_placement(0,_,_,Dict)->
    [Pos || {Pos,_} <- dict:to_list(Dict)];
base_placement(Player, Map, {Width, Height}, Dict) ->
    X = random:uniform(Width),
    Y = random:uniform(Height),
    case ok_tile({X,Y}, {Width, Height}, Map, Dict) of
	false ->
	    base_placement(Player, Map, {Width, Height}, Dict);
	true ->	
	    get_free_neighbour({X,Y}, {Width, Height}, Map, Dict),
	    base_placement(Player-1,Map,{Width, Height},dict:store({X,Y},Player,Dict))
    end.

%implement!
get_free_neighbour({_X,_Y}, _MapWH, _Map, _Dict) ->
    nope.

ok_tile({X, _Y}, {Width, _Height}, _Map, _Dict) when (X > Width) or (X < 1) ->
    false;
ok_tile({_X, Y}, {_Width, Height}, _Map, _Dict) when (Y > Height) or (Y < 1) ->
    false;
ok_tile(Pos, _MapWH, Map, Dict) ->
    case is_water(Pos, Map) of
	true -> false;
	false ->
	    case dict:is_key(Pos, Dict) of 
		false -> 
		    true;
		true ->
		    false
	    end
    end.

is_water({X, Y}, Map) ->
    case element(Y,element(X,Map)) of
	"Sea" -> true;
	"Ocean" -> true;
	_ -> false
    end.
