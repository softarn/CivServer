-define(TCP, tcp).

-record(player,{
	ref,
	handler,
	name,
	socket}).

-record(game,{
	name,
	game_pid}).
