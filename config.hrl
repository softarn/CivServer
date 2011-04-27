-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 0).
-define(SERVER, server). 

%Player record
-record(player,{
	ref,
	handler,
	name,
	socket}).

%Game record
-record(game,{
	name,
	game_pid,
	players}).
