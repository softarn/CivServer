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


% Arguments: Player that wants to create a game
% If there already exists a game with the same name as the host
%   Returns failed
% Else
%   Creates a new game
%   Returns the new game and adds it to the server loopdata

handle_call({create_game, Player}, _From, {Games, Players}) ->
    Find_Existing_Game = fun(GR) ->
	    GR#game.name =:= Player#player.name
    end,

    case length(lists:filter(Find_Existing_Game, Games)) of

	0 ->
	    Game = ?GAMESRV:start(Player), 
	    io:format("Created new game\n"),
	    {reply, Game, {[Game|Games], Players}};

	_ -> %Game name already exists!
	    {reply, failed, {Games, Players}}
	end;

handle_call(list_games, _From, {Games, Players}) ->
    FormatName = fun(G) ->
	    case G#game.locked of
		0 ->
		    G#game.name;
		_ ->
		    G#game.name
	    end
    end,
    GameList = [FormatName(Game) || Game <- Games, Game#game.current_state =:= game_lobby],
    {reply, GameList, {Games,Players}};


% Arguments: Player to add
% If the playername already exists:
%   Returns false
% Else
%   Adds player to server
%   Returns true

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

handle_cast({update_game, Game}, {Games, Players}) ->
    io:format("Updating game~n"),
    Find_Game = fun(GR) ->
	    GR#game.game_pid =/= Game#game.game_pid
    end,
    NewGameList = lists:filter(Find_Game, Games),
    NewGames = [Game | NewGameList],
    {noreply, {NewGames, Players}};

handle_cast({remove_game, Game}, {Games, Players}) ->
    io:format("Removing game~n"),
    Find_Game = fun(GR) ->
	    GR#game.game_pid =/= Game#game.game_pid
    end,
    UpdatedGames = lists:filter(Find_Game, Games),
    {noreply, {UpdatedGames, Players}}; 

handle_cast({rm_player, {socket, Socket}}, {Games, Players}) ->
    Find_Player = fun(P) ->
	    P#player.socket =:= Socket
    end,

    In_game= fun(Game) ->
	    List = lists:filter(Find_Player, Game#game.players),
	    case length(List) of
		0 ->
		    false;
		1 ->
		    true
	    end
    end,

    case lists:filter(Find_Player, Players) of
	[OldPlayer] ->

	    PerhapsGame = lists:filter(In_game, Games),

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
	    {noreply, {Games, NewPlayers}};
	_ ->
	    {noreply, {Games, Players}}
    end.

terminate(_Reason, _State) ->
    %Kill con_handler?
    ok.

%Server calls and casts
%Player
add_player(Player) -> gen_server:call(?SERVER, {add_player, Player}).
list_players() -> gen_server:call(?SERVER, list_players).
rm_player({socket, Socket}) -> gen_server:cast(?SERVER, {rm_player, {socket, Socket}}).
%Game
update_game(Game) -> gen_server:cast(?SERVER, {update_game, Game}).
create_game(Host) -> gen_server:call(?SERVER, {create_game, Host}).
list_games() -> gen_server:call(?SERVER, list_games).
get_game(GameName) -> gen_server:call(?SERVER, {get_game, GameName}).
remove_game(Game) -> gen_server:cast(?SERVER, {remove_game, Game}).

%Server
stop() -> gen_server:call(?SERVER, stop).
