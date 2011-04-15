-module(testserver1).

-export([init/0, accept/2]).

init() ->
	{ok, ListenSocket} = gen_tcp:listen(1234, [binary, {active, false}]),
	accept(ListenSocket, 0).

accept(ListenSocket, Count) ->
	{ok, Socket} = gen_tcp:accept(ListenSocket),
	io:format("Accepted\n"),
	spawn(?MODULE, accept, [ListenSocket, Count+1]),
	recv(Socket).

decodeHeader(<<Header:8, Rest/binary>>) -> %unsigned, TODO: FIXA SIGNED!
	io:format("~w\n", [Header]),
	io:format("~w\n", [Rest]),
	{Header, Rest}.

decodeInteger(<<Int:32, Rest/binary>>) ->
	{Int, Rest}.

recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Data} -> %Data = Binärt
			io:format("tog emot ok, Data\n"),
			{Header, Rest} = decodeHeader(Data),
			case Header of 
				2 ->
					{Int, Rest2} = decodeInteger(Rest),
					io:format("Hello World! Versionsnr: "),
					io:format("~w\n", [Int])
			end,
			recv(Socket);

		{error,closed} ->
			io:format("Closed connection\n")
	end.
