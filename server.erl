-module(server).
-behaviour(gen_server).

-compile(export_all).
-export([start_link/1, init/1]).
-export([add_player/1]).

-include("config.hrl").

%Startup
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []). % 1:locally/globally registered servername, 2: Callback module, 3: Arguments, 4: Options

init(Port) ->
    spawn(con_handler, start, [Port]),
    {ok, {[], []}}. %LoopData/State (Games, Players)

%Callbacks
handle_call({add_player, Player}, _From, {Games, Players}) ->
    io:format("Added player ~w~n", [Player]), %Glöm ej felkontroll ifall player existerar!
    {reply, ok, {Games, [Player|Players]}};

handle_call(list_players, _From, {Games, Players}) ->
    {reply, [Player#player.name || Player <- Players], {Games,Players}};

handle_call({create_game, Player}, _From, {Games, Players}) ->
    ?GAMESRV:start(self(), Player), % gör nytt spel EJ LINK ???
    {reply, ok, {[Games], Players}};

handle_call(list_games, _From, {Games, Players}) ->
    {reply, [Game#game.name || Game <- Games], {Games,Players}};

handle_call({add_player, Player}, _From, {Games, Players}) -> %Check if playerId already exists
    
    ListOfPlayerNames = [NewPlayer#player.name || NewPlayer <- Players],
    Pfun = fun(OldPlayer) ->
	      Player#player.name == OldPlayer
    end,
	    		
    case lists:any(Pfun, ListOfPlayerNames) of 
       true ->	   
	    io:format("Playername already exists ~w~n",[Player]),
    	    {reply, false, {Games, Players}};%MAGNUS!!!! TA HAND OM DETTA SÅ ATT DET SKICKAS ETT FELMEDD.
	 false ->	   
    	    io:format("Added player ~w~n", [Player]),
    	    {reply, true, {Games, [Player|Players]}}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, shutdown_ok, State}.

handle_cast({add_game, Game}, _From, {Games, Players}) ->
    {noreply, {[Game|Games], Players}}.

terminate(_Reason, _State) ->
    %Kill con_handler?
    ok.

%Server calls and casts
add_player(Player) ->  
    PlayerName = string:to_lower(Player),
    gen_server:call(?MODULE, {add_player, PlayerName}).
	  	  

list_players() -> gen_server:call(?MODULE, list_players).
create_game(Host) -> gen_server:call(?MODULE, {create_game, Host}).
list_games() -> gen_server:call(?MODULE, list_games).
stop() -> gen_server:call(?MODULE, stop).

add_game(Game) -> gen_server:cast(?MODULE, {add_game, Game}).

