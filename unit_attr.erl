-module(unit_attr).
-export([get_attr/1,create_unit/1]).

-include("config.hrl").

create_unit(UnitType) ->
    case UnitType of
	"Catapult" ->	#unit{type="Catapult",name=catapult,mp=100,ap=12,dp=1,range=2,movement=1,att_type=bombardment,move_type=ground};		
	"Trebuchet" ->	#unit{type="Trebuchet",name=trebuchet,mp=100,ap=20,dp=2,range=3,movement=1,att_type=bombardment,move_type=ground};		
	"Cannon" ->	#unit{type="Cannon",name=cannon,mp=100,ap=30,dp=3,range=4,movement=1,att_type=bombardment,move_type=ground};		
	"Archer" ->	#unit{type="Archer",name=archer,mp=100,ap=4,dp=2,range=2,movement=1,att_type=range,move_type=ground};		
	"Musketeer" ->	#unit{type="Musketeer",name=musketeer,mp=100,ap=8,dp=6,range=2,movement=1,att_type=range,move_type=ground};		

	"Phalanx" ->	#unit{type="Phalanx",name=phalanx,mp=100,ap=2,dp=5,range=1,movement=1,att_type=assault,move_type=ground};		
	"Legion" ->	#unit{type="Legion",name=legion,mp=100,ap=6,dp=4,range=1,movement=1,att_type=assault,move_type=ground};		
	"Infantry" ->	#unit{type="Infantry",name=infantry,mp=100,ap=3,dp=3,range=1,movement=1,att_type=assault,move_type=ground};		
	"Pikeman" ->	#unit{type="Pikeman",name=pikeman, mp=100,ap=2,dp={3,12},range=1,movement=1,att_type=assault,move_type=ground};		

	"Cavalry" ->	#unit{type="Cavalry",name=cavalry,mp=100,ap=6,dp=4,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Knight" ->	#unit{type="Knight",name=knight,mp=100,ap=12,dp=8,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Crusader" ->	#unit{type="Crusader",name=crusader,mp=100,ap=18,dp=12,range=1,movement=2,att_type=assault,move_type=mounted};		

	"Trireme" ->	#unit{type="Trireme",name=trireme,mp=50,ap=4,dp=3,range=1,movement=3,att_type=range,move_type=naval};		
	"Galley" ->	#unit{type="Galley",name=galley,mp=250,ap=30,dp=25,range=2,movement=3,att_type=bombardment,move_type=naval};		
	"Caravel" ->	#unit{type="Caravel",name=caravel,mp=100,ap=50,dp=40,range=3,movement=6,att_type=bombardment,move_type=naval};
	_-> {error, invalid_unit}
    end.

get_attr(Unit)->
    case Unit of
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

	trireme ->	{50,4,3,1,3,range,naval};		
	galley ->	{250,30,25,2,3,bombardment,naval};		
	caravel ->	{100,50,40,3,6,bombardment,naval};
	_-> {error, invalid_unit}
    end.
