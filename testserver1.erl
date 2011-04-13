-module(testserver1).

-export([init/0, accept/2]).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(4711, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted\n"),
        spawn(?MODULE, accept, [ListenSocket, Count+1]),
	recv(Socket).

recv(Socket) ->
      	case gen_tcp:recv(Socket, 0) of
          {ok, Data} ->
            io:format("~p\n", [Data]),
            recv(Socket);

	  {error,closed} ->
	    io:format("Closed connection\n")
	end.
