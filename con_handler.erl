-module(con_handler).

-export([start/2]).
-include("config.hrl").

start(Port, Parent) ->
    init(Port, Parent).

init(Port, Parent) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [binary, {active, false}]),
    accept(ListenSocket, Parent).

accept(ListenSocket, Parent) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    io:format("Accepted Connection\n"),
    spawn(?P_HANDLER, recv_player, [Socket, Parent]),
    accept(ListenSocket, Parent).
