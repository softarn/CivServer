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
	    {ok, FSM_Pid} = ?P_FSM:start(),
	    io:format("Startat FSM"),
	    Player = #player{socket = Socket, ref = make_ref(), fsm_pid = FSM_Pid},
	    ?P_FSM:connect(FSM_Pid, Player),
	    io:format("Connectat FSM");
	{error, enfile} -> 
	    io:format("Denied Connection\n")
    end,
    accept(ListenSocket).
