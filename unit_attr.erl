-module(unit_attr).
-export([get_attr/1,create_unit/2,get_unit_info/1]).

-include("config.hrl").

create_unit(UnitType, Owner) ->
    case UnitType of
	"Catapult" ->	#unit{str="Catapult",	owner=Owner,	name=catapult,	mp=100};
	"Trebuchet" ->	#unit{str="Trebuchet",	owner=Owner,	name=trebuchet,	mp=100};
	"Cannon" ->	#unit{str="Cannon",	owner=Owner,	name=cannon,	mp=100};
	"Archer" ->	#unit{str="Archer",	owner=Owner,	name=archer,	mp=100};
	"Musketeer" ->	#unit{str="Musketeer",	owner=Owner,	name=musketeer,	mp=100};

	"Phalanx" ->	#unit{str="Phalanx",	owner=Owner,	name=phalanx,	mp=100};
	"Legion" ->	#unit{str="Legion",	owner=Owner,	name=legion,	mp=100};
	"Infantry" ->	#unit{str="Infantry",	owner=Owner,	name=infantry,	mp=100};
	"Pikeman" ->	#unit{str="Pikeman",	owner=Owner,	name=pikeman,	mp=100};

	"Cavalry" ->	#unit{str="Cavalry",	owner=Owner,	name=cavalry,	mp=100};
	"Knight" ->	#unit{str="Knight",	owner=Owner,	name=knight,	mp=100};
	"Crusader" ->	#unit{str="Crusader",	owner=Owner,	name=crusader,	mp=100};

	"Settler" -> 	#unit{str="Settler", 	owner=Owner, 	name=settler, mp=100};		

	"Trireme" ->	#unit{str="Trireme",	owner=Owner,	name=trireme,	mp=50};
	"Galley" ->	#unit{str="Galley",	owner=Owner,	name=galley,	mp=250};
	"Caravel" ->	#unit{str="Caravel",	owner=Owner,	name=caravel,	mp=100};

	"Siege Tower" -> #unit{str="Siege Tower",owner=Owner,	name=siege_tower,mp=0};
	_-> {error, invalid_unit}
    end.

get_unit_info(UnitType) ->
    case UnitType of
	"Catapult" ->	#unit_info{name=catapult,mp=100,ap=12,dp=1,range=2,movement=1,att_type=bombardment,move_type=ground};
	"Trebuchet" ->	#unit_info{name=trebuchet,mp=100,ap=20,dp=2,range=3,movement=1,att_type=bombardment,move_type=ground};
	"Cannon" ->	#unit_info{name=cannon,mp=100,ap=30,dp=3,range=4,movement=1,att_type=bombardment,move_type=ground};
	"Archer" ->	#unit_info{name=archer,mp=100,ap=4,dp=2,range=2,movement=1,att_type=range,move_type=ground};
	"Musketeer" ->	#unit_info{name=musketeer,mp=100,ap=8,dp=6,range=2,movement=1,att_type=range,move_type=ground};

	"Phalanx" ->	#unit_info{name=phalanx,mp=100,ap=2,dp=5,range=1,movement=1,att_type=assault,move_type=ground};
	"Legion" ->	#unit_info{name=legion,mp=100,ap=6,dp=4,range=1,movement=1,att_type=assault,move_type=ground};
	"Infantry" ->	#unit_info{name=infantry,mp=100,ap=3,dp=3,range=1,movement=1,att_type=assault,move_type=ground};
	"Pikeman" ->	#unit_info{name=pikeman, mp=100,ap=2,dp={3,12},range=1,movement=1,att_type=assault,move_type=ground};

	"Cavalry" ->	#unit_info{name=cavalry,mp=100,ap=6,dp=4,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Knight" ->	#unit_info{name=knight,mp=100,ap=12,dp=8,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Crusader" ->	#unit_info{name=crusader,mp=100,ap=18,dp=12,range=1,movement=2,att_type=assault,move_type=mounted};		

	"Settler" -> 	#unit_info{name=settler,mp=100,ap=0,dp=2,range=0,movement=1,att_type=assault,move_type=ground};		

	"Trireme" ->	#unit_info{name=trireme,mp=50,ap=4,dp=3,range=1,movement=3,att_type=range,move_type=naval};		
	"Galley" ->	#unit_info{name=galley,mp=250,ap=30,dp=25,range=2,movement=3,att_type=bombardment,move_type=naval};		
	"Caravel" ->	#unit_info{name=caravel,mp=100,ap=50,dp=40,range=3,movement=6,att_type=bombardment,move_type=naval};
	_-> {error, invalid_unit}
    end.

get_attr(Unit)->
    case Unit of	% MP,AP,DP,Rng,Mov,Atk,MovT
	catapult ->	{100,12,1,2,1,bombardment,ground};		
	trebuchet ->	{100,20,2,3,1,bombardment,ground};		
	cannon ->	{100,30,3,4,1,bombardment,ground};		
	archer ->	{100,4,2,2,1,range,ground};		
	musketeer ->	{100,8,6,2,1,range,ground};		

	phalanx ->	{100,2,5,1,1,assault,ground};		
	legion ->	{100,6,4,1,1,assault,ground};		
	infantry ->	{100,3,3,1,1,assault,ground};		
	pikeman ->	{100,2,{3,12},1,1,assault,ground};		

	cavalry ->	{100,6,4,1,2,assault,mounted};		
	knight ->	{100,12,8,1,2,assault,mounted};		
	crusader ->	{100,18,12,1,2,assault,mounted};

	settler -> 	{100,0,2,0,1,assault,ground};

	trireme ->	{50,4,3,1,3,range,naval};		
	galley ->	{250,30,25,2,3,bombardment,naval};		
	caravel ->	{100,50,40,3,6,bombardment,naval};
	siege_tower ->  {0, 0, 0, 0, 1, assault, ground};
	_-> {error, invalid_unit}
    end.
