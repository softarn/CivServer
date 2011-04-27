-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 0).
-define(SERVER, server). 
-record(player,{
	ref,
	handler,
	name,
	socket}).

-record(game,{
	name,
	game_pid,
	players}).
