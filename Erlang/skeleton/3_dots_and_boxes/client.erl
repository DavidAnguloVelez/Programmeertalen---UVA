%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis van een speler in
%   het spel 'kamertje verhuur'.

-module(client).
-export([move/0, new/0]).


move() ->
    <<S1:32, S2:32, S3:32>> = crypto:strong_rand_bytes(12),
    rand:seed(exsplus,{S1, S2, S3}),
    receive
        finished ->
            io:format("~p: I am done~n", [self()]);

        {move, ServerPid, Grid} ->
            Wall = grid:choose_random_wall(Grid),
            gen_server:call(ServerPid, {move, Wall}),
            move()
    end.


new() ->
    spawn(client, move, []).
