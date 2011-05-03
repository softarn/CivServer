-module(con_handler).

-export([start/1]).
-include("config.hrl").

start(Port) ->
    init(Port).

init(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    case gen_tcp:accept(ListenSocket) of
	{ok, Socket} -> 
	    io:format("Accepted Connection\n"),
	    %spawn(?P_HANDLER, init, [Socket]),
	    Player = #player{socket = Socket, ref = make_ref()},
	    spawn(?P_FSM, start, [Player]);
	{error, enfile} -> 
	    io:format("Denied Connection\n")
    end,
    accept(ListenSocket).
