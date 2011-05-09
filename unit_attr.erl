-module(unit_attr).
-export([get_attr/1,create_unit/1]).

-include("config.hrl").

create_unit(UnitType) ->
    case UnitType of
	"Catapult" ->	#unit{name=catapult,manpower=100,att_points=12,def_points=1,range=2,movement=1,att_type=bombardment,move_type=ground};		
	"Trebuchet" ->	#unit{name=trebuchet,manpower=100,att_points=20,def_points=2,range=3,movement=1,att_type=bombardment,move_type=ground};		
	"Cannon" ->	#unit{name=cannon,manpower=100,att_points=30,def_points=3,range=4,movement=1,att_type=bombardment,move_type=ground};		
	"Archer" ->	#unit{name=archer,manpower=100,att_points=4,def_points=2,range=2,movement=1,att_type=range,move_type=ground};		
	"Musketeer" ->	#unit{name=musketeer,manpower=100,att_points=8,def_points=6,range=2,movement=1,att_type=range,move_type=ground};		

	"Phalanx" ->	#unit{name=phalanx,manpower=100,att_points=2,def_points=5,range=1,movement=1,att_type=assault,move_type=ground};		
	"Legion" ->	#unit{name=legion,manpower=100,att_points=6,def_points=4,range=1,movement=1,att_type=assault,move_type=ground};		
	"Infantry" ->	#unit{name=infantry,manpower=100,att_points=3,def_points=3,range=1,movement=1,att_type=assault,move_type=ground};		
	"Pikeman" ->	#unit{name=pikeman, manpower=100,att_points=2,def_points={3,12},range=1,movement=1,att_type=assault,move_type=ground};		

	"Cavalry" ->	#unit{name=cavalry,manpower=100,att_points=6,def_points=4,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Knight" ->	#unit{name=knight,manpower=100,att_points=12,def_points=8,range=1,movement=2,att_type=assault,move_type=mounted};		
	"Crusader" ->	#unit{name=crusader,manpower=100,att_points=18,def_points=12,range=1,movement=2,att_type=assault,move_type=mounted};		

	"Trireme" ->	#unit{name=trireme,manpower=50,att_points=4,def_points=3,range=1,movement=3,att_type=range,move_type=naval};		
	"Galley" ->	#unit{name=galley,manpower=250,att_points=30,def_points=25,range=2,movement=3,att_type=bombardment,move_type=naval};		
	"Caravel" ->	#unit{name=caravel,manpower=100,att_points=50,def_points=40,range=3,movement=6,att_type=bombardment,move_type=naval};
	_-> {error, invalid_unit}
    end.

get_attr(Unit)->
    case Unit of
	catapult ->	{100,12,1,2,1,bombardment,ground};		
	trebuchet ->{100,20,2,3,1,bombardment,ground};		
	cannon ->	{100,30,3,4,1,bombardment,ground};		
	archer ->	{100,4,2,2,1,range,ground};		
	musketeer ->{100,8,6,2,1,range,ground};		

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
