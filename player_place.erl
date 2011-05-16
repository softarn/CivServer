-module(player_place).
-compile(export_all).


get_placement(Players,Map) when is_list(Map) ->
	%Width = length(Map)
	get_base_pos(Players,list_to_tuple([list_to_tuple(X) || X<- Map])).



get_base_pos(Players,Map)->
	Width = size(element(1,Map)),
	Height = size(Map),	
	base_placement(Players,Map,Width,Height,dict:new()).

base_placement(0,_,_,_,Dict)->
	[{X,Y}|| {{X,Y},_} <-dict:to_list(Dict)];
base_placement(Player,Map,Width,Height,Dict)->
	X = random:uniform(Width),
	Y = random:uniform(Height),
	case element(Y,element(X,Map)) of
	"Sea"-> 
		base_placement(Player,Map,Width,Height,Dict);
	"Ocean"-> 
		base_placement(Player,Map,Width,Height,Dict);
	_->	
		case dict:find({X,Y},Dict) of 
			error -> 
				base_placement(Player-1,Map,Width,Height,dict:store({X,Y},Player,Dict));
			_->
				base_placement(Player,Map,Width,Height,Dict)
		end
	end.
	
	
	
