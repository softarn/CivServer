-module(unit_attr).
-compile(export_all).



get_attr(Unit)->
	case Unit of
		catapult ->	{100,12,1,2,1,bombardment};		
		trebuchet ->{100,20,2,3,1,bombardment};		
		cannon ->	{100,30,3,4,1,bombardment};		
		archer ->	{100,4,2,2,1,range};		
		musketeer ->{100,8,6,2,1,range};		

		phalanx ->	{100,2,5,1,1,assault};		
		legion ->	{100,6,4,1,1,assault};		
		infantry ->	{100,3,3,1,1,assault};		
		pikeman ->	{100,2,{3,12},1,1,assault};		

		cavalry ->	{100,6,4,1,2,assault};		
		knight ->	{100,12,8,1,2,assault};		
		crusader ->	{100,18,12,1,2,assault};		
