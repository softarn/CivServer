-module(combat).
-compile(export_all).


get_range({X1,Y1},{X2,Y2})->
	Z1 = X1 - Y1,		
	Z2 = X2 - Y2,		
	erlang:max(erlang:max(dif(X1,X2),dif(Y1,Y2)),dif(Z1,Z2)).
dif(X1,X2)->
	erlang:max(X1,X2) - erlang:min (X1,X2).
	
