%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis van een speler in
%   het spel 'kamertje verhuur'.

-module(client).
-export([move/0, new/0]).

% Gets called when player needs to make a move in the game 'rent a room'.
move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus,{S1, S2, S3}),
    receive
        % Signal received when the game is finished.
        finished ->
            io:format("~p: I am done~n", [self()]);

        % Signal received when player needs to make a move.
        {move, ServerPid, Grid} ->
            case length(grid:get_completable_walls(Grid)) of
                0 ->
                    Wall = grid:choose_random_wall(Grid);
                _ ->
                    [Wall | _] = grid:get_completable_walls(Grid)
            end,
            gen_server:call(ServerPid, {move, Wall}),
            move()
    end.

% Initialises a new player/client.
new() ->
    spawn(client, move, []).
