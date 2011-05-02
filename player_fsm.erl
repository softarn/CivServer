-module(player_fsm).
-behaviour(gen_fsm).

-include("config.hrl").

-compile(export_all).

start(Player) ->
	gen_fsm:start({local, list_to_atom(erlang:ref_to_list(Player#player.ref))}, ?MODULE, Player, []).

init(Player) ->
	{ok, connect, Player}. % 2. first state, 3: fsm-data

connect() ->
	k.
