-module(server).
-behaviour(gen_server).

-compile(export_all).
-export([start_link/1, init/1]).
-export([add_player/1]).

-include("config.hrl").

%Startup
start_link(Port) ->
    % 1: Locally registered servername, 2: Callback module, 3: Arguments, 4: Options
    gen_server:start_link({local, ?SERVER}, ?MODULE, Port, []). 
init(Port) ->
    spawn(con_handler, start, [Port]),
    {ok, {[], []}}. %LoopData/State (Games, Players)

%Callbacks
handle_call(list_players, _From, {Games, Players}) ->
    {reply, [Player#player.name || Player <- Players], {Games,Players}};

handle_call({create_game, Player}, _From, {Games, Players}) ->
    Game = ?GAMESRV:start(Player), 
    io:format("Created new game\n"),
    {reply, Game, {[Game|Games], Players}};

handle_call(list_games, _From, {Games, Players}) ->
    {reply, [Game#game.name || Game <- Games], {Games,Players}};

handle_call({add_player, Player}, _From, {Games, Players}) -> 

    ListOfPlayerNames = [NewPlayer#player.name || NewPlayer <- Players],
    Pfun = fun(P) ->
	    Player#player.name == P
    end,

    case lists:any(Pfun, ListOfPlayerNames) of 
	true ->	   
	    io:format("Playername already exists ~w~n",[Player]),
	    {reply, false, {Games, Players}};
	false ->	   
	    io:format("Added player ~w~n", [Player]),
	    {reply, true, {Games, [Player|Players]}}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State};

handle_call({update_game, Game}, _From, {Games, Players}) ->
    io:format("Updating game~n"),
    Find_Game = fun(GR) ->
	    GR#game.game_pid =/= Game#game.game_pid
    end,
    NewGameList = lists:filter(Find_Game, Games),
    NewGames = [Game | NewGameList],
    {reply, ok, {NewGames, Players}};

handle_call({get_game, GameName}, _From, {Games, Players}) ->
    Fun = fun(X) ->
	    X#game.name =:= GameName 
    end,
    case lists:filter(Fun, Games) of
	[] ->
	    {reply, [], {Games, Players}};
	[Game] ->
	    {reply, Game, {Games, Players}}
    end.

handle_cast({rm_player, {socket, Socket}}, {Games, Players}) ->
    Find_Player = fun(P) ->
	    P#player.socket =:= Socket
    end,

    [OldPlayer] = lists:filter(Find_Player, Players),

    Something = fun(Game) ->
	    Find_P_layer = fun(PRecord) ->
		    OldPlayer =:= PRecord
	    end,
	    List = lists:filter(Find_P_layer, Game#game.players),
	    case length(List) of
		0 ->
		    false;
		1 ->
		    true
	    end
    end,
    
    PerhapsGame = lists:filter(Something, Games),

    case length(PerhapsGame) of
	    0 ->
		ok;
	    1 ->
		[TheGame] = PerhapsGame,
		?GAMESRV:player_leave(TheGame#game.game_pid, OldPlayer);
	    _ ->
		io:format("Fel! En spelare är med i flera spel..")
	end,

    Find_Other_Players = fun(P) ->
	    P#player.socket =/= Socket
    end,

    NewPlayers = lists:filter(Find_Other_Players, Players),
    io:format("Deleted a player~n"),
    {noreply, {Games, NewPlayers}}.

terminate(_Reason, _State) ->
    %Kill con_handler?
    ok.

%Server calls and casts
%Player
add_player(Player) -> gen_server:call(?SERVER, {add_player, Player}).
list_players() -> gen_server:call(?SERVER, list_players).
rm_player({socket, Socket}) -> gen_server:cast(?SERVER, {rm_player, {socket, Socket}}).
%Game
update_game(Game) -> gen_server:call(?SERVER, {update_game, Game}).
create_game(Host) -> gen_server:call(?SERVER, {create_game, Host}).
list_games() -> gen_server:call(?SERVER, list_games).
get_game(GameName) -> gen_server:call(?SERVER, {get_game, GameName}).

%Server
stop() -> gen_server:call(?SERVER, stop).
