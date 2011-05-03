-module(player_fsm).
-behaviour(gen_fsm).

-include("config.hrl").

-compile(export_all).

start() ->
    gen_fsm:start(?MODULE, [], []).

init() ->
    {ok, connecting, []}. % 2. first state, 3: fsm-data

%States
connecting(Player, _State) ->
    {ok, Pid} = spawn_link(?P_HANDLER, init, [Player#player.socket]), %skicka med pid
    NewPlayer = Player#player{handler_pid = Pid},
    {new_state, lobby, NewPlayer}.

getting_p_info({Header, List}, State) ->
    case Header of
	2 ->
	    [Name, P_version] = List,
	    case P_version =:= ?P_VERSION of
		true -> 
		    NewPlayer = Player#player{name = Name} %Create player
%lines under this needs to be fixed
		    case ?SERVER:add_player(Player) of
			true ->
			    ?TCP:sendHeader(Socket, 3), % Welcome to the Real world
			    recv_lobby(Player);
			false ->
			    ?TCP:sendFailPacket(Socket, 1, Header), %Fail packet name already exists
			    recv_player(Socket)
		    end;

		false ->
		    ?TCP:readString(Socket),
		    ?TCP:sendFailPacket(Socket, 0, Header) %Send fail packet, wrong protocolversion
	    end;
%end of line fixing :)


%Event sending functions
connect(Pid, Player) ->
    gen_fsm:sync_send_event(Pid, Player).
