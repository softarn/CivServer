-module(con_handler).

-export([start/2]).
-include("config.hrl").

start(Port, Parent) ->
    init(Port),
    loop(Parent).

init(Port) ->
   spawn(tcp, init, [self(), Port]).

loop(Parent) ->
    receive 
	Socket ->
	    case ?PARSER:recv_pname(Socket) of
		{ok, PlayerName} -> 
		    Parent ! Parent:add_player(#player{name = PlayerName,
			    socket = Socket});
		{error, Reason} ->
		    io:format("Error accepting new player: ~w~n", [Reason])
	    end
    end,
    loop(Parent).
