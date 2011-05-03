-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 0).
-define(SERVER, server).
-define(GAMESRV, gameserver).
-define(TERGEN, ter_gen).
-define(P_FSM, player_fsm).

-record(player,{
	ref,
	handler_pid,
	fsm_pid,
	name,
	socket,
	civ}).

-record(game,{
	name,
	game_pid,
	players,
	locked,
	map}).
