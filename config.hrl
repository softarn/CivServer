-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 0).
-define(SERVER, server).
-define(GAMESRV, gameserver).

-record(player,{
	ref,
	handler,
	name,
	socket,
	civ}).

-record(game,{
	name,
	game_pid,
	players,
	locked}).
