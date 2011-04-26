-define(PARSER, parser).

-record(player,
    {name,
    socket}).

-record(game,
    {name,
    game_pid}).
