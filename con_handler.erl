-module(con_handler).

-export([init/2]).

-include("config.hrl").

init(Port, Parent) ->
   spawn(parser, init, [self(), Port]),
   loop(Parent).

loop(Parent) ->
    receive 
	Socket ->
	    case ?PARSER:recv_pname(Socket) of
		{ok, PlayerName} -> 
		    Parent ! {new_player, #player{name = PlayerName,
			    socket = Socket}};
		{error, Reason} ->
		    io:format("Error accepting new player: ~w~n", [Reason])
	    end
    end,
    loop(Parent).
