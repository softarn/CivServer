-module(testserver1).

-export([init/0, accept/2]).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1233, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted\n"),
	spawn(?MODULE, accept, [ListenSocket, Count+1]),
	recv(Socket).

recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> %Data = Binärt
			[Header|_T] = bitstring_to_list(Data), %Header = first byte of received packet as integer
			io:format("~w\n", [bit_size(<<Header>>)]),
			io:format("~w\n", [Header]),
			case Header of
				1 ->
					io:format("~p\n", ["Confirm'd"]);

				2 ->
					io:format("~p\n", ["Hello World"]);
				_ -> 
					io:format("~p\n", ["Nope"])
			end,
			recv(Socket);

		{error,closed} ->
			io:format("Closed connection\n")
	end.
