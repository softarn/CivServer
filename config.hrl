-define(TCP, tcp).

-record(player,
    {name,
    socket}).

-record(game,
    {name,
    game_pid}).
