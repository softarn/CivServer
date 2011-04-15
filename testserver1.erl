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

fail(FailureType) ->
	case FailureType of
		0 ->
			Msg = "Invalid Protocol Version"
	end,
	Msg.

recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Packet} -> %Data = Binärt
			io:format("tog emot ok, Data\n"),
			{Header, RestOfMsg} = decodeHeader(Packet),
			case Header of 
				2 ->
					{Int, RestOfMsg2} = decodeInteger(RestOfMsg),
					case Int =:= 0 of
						true ->
							Msg = list_to_binary([term_to_binary(3)]),
							gen_tcp:send(Socket, Msg);
						false ->
							io:format("false"),
							FailMsg = list_to_binary([term_to_binary(0), term_to_binary(0), term_to_binary(fail(0))]), 
							gen_tcp:send(Socket, FailMsg),
							io:format("skickat")
					end
			end,
			recv(Socket);

		{error,closed} ->
			io:format("Closed connection\n")
	end.
