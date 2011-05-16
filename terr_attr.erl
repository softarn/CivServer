-module(terr_attr).
-export([get_attr/1]).

get_attr(Terrain)->
    case Terrain of
	"sea" ->		{0.50, 0};
	"ocean" ->		{0.30, 0.20};
	
	"plains" ->		{0.25, 0};
	"grassland" ->	{0.25, 0};
	"marsh" ->		{0, 0.25};
	"desert" ->		{0.10, 0};
	"tundra" ->		{0.15, 0};
	
	"rain forrest" -> 		{0.15, 0.50};
	"conifer forrest" ->	{0.25, 0.75};
	"broadleaf forrest" ->	{0.25, 0.75};
	
	"hills" ->		{0.25, 0.100};
	"mountains" -> 	{0.20, 0.200};

	_-> {error, invalid_unit}
    end.