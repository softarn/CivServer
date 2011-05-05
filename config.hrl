-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 2).
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
	map,
	tilelist}).

-record(tile, {
	position = null, %{X,Y}
	unit = null, %unitrecord
	city = null, %cityrecord
	improvement = null}). %String

-record(city, {
	owner = null, %String
	units = null, % [Units]
	buildings = null, %[String]
	name = null}). %String

-record(unit, {
	owner = null, %String
	type = null, %String
	manpower = null}). %Integer
