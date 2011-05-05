-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 2).
-define(SERVER, server).
-define(GAMESRV, gameserver).
-define(TERGEN, ter_gen).
-define(P_FSM, player_fsm).

-record(player,{
	ref = null,
	handler_pid = null,
	fsm_pid = null,
	name = null,
	socket = null,
	civ = null}).

-record(game,{
	name = null,
	game_pid = null,
	players = null,
	locked = null,
	current_state = null, %States = game_lobby, in_game
	map = null,
	tilelist = null}).

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
