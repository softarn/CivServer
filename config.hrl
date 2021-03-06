-define(TCP, tcp).
-define(P_HANDLER, player_handler).
-define(P_VERSION, 3).
-define(SERVER, server).
-define(GAMESRV, gameserver).
-define(TERGEN, ter_gen).
-define(P_FSM, player_fsm).
-define(GAMEPLAN, gameplan).
-define(START_POS, player_place).
-define(U_HANDLER, unit_attr).
-define(COMBAT, combat_eval).
-define(BOMB, city_bombard).

-record(player,{
	start_pos = null, %{X,Y}
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
	tilemap = null}).

-record(tile, {
	position = null, %{X,Y}
	unit = null, %unitrecord
	city = null}). %cityrecord

-record(city, {
	owner = null, %String
	units = [],	% [Units]
	name = null}). %String

-record(unit_info, {
	name = null, %String
	mp = null, %Integer
	ap = null,
	dp = null,
	range = null,
	movement = null,
	att_type = null,
	move_type = null}).

-record(unit, {
	owner = null,
	name = null,
	str = null,
	units = [],
	fortified = false,
	mp = null}).
