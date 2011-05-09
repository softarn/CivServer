-module(unit_attr).
-export([get_attr/1]).



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
