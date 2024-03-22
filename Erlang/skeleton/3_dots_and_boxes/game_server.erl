%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis van een game server voor het
%   spel 'kamertje verhuur'.

-module(game_server).

-include_lib("eunit/include/eunit.hrl").

-behaviour(gen_server).

-export([start_link/1, handle_call/3, handle_cast/2]).
-export([init/1, move/2]).

start_link({W, H, Players}) ->
    gen_server:start_link(game_server, {W, H, Players}, []).

% Abstraction to make a move.
move(Pid, Wall) ->
    gen_server:call(Pid, {move, Wall}).


% Initiliases a game server, checks if there are more than 0 players to start
% the game.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    case length(Players) of
        0 ->
            {error, "Not enough players"};
        _ ->
            [Player | _] = Players,
            Player ! {move, self(), Grid},
            {ok, {Grid, Players}}
    end.

% Returns and calculates the score given the wall that is just placed in the grid.
calculate_score(Wall, Grid) ->
    {Cell1, Cell2} = Wall,

    % Check if first cell has been filled.
    case grid:check_cell_filled(Cell1, Grid) of
        true ->
            Score = 1;

        false ->
            Score = 0
    end,

    % Check if second cell has been filled.
    case grid:check_cell_filled(Cell2, Grid) of
        true ->
            Score1 = Score + 1;

        false ->
            Score1 = Score
    end,


    Score1.

% Handles the call whenever a player makes a move, calculates and replies with
% the score made with the given move and determines the sequence of turns.
% Replies with a move call to the player which turn it is, replying with the
% new grid.
handle_call({move, Wall}, _From, {Grid, Players}) ->
    GridNew = grid:add_wall(Wall, Grid),
    [Player | PlayersRest] = Players,

    Score = calculate_score(Wall, GridNew),

    % Checks if a player gained a point.
    if
        Score > 0 ->
            NewPlayers = [Player] ++ PlayersRest;

        true ->
            NewPlayers = PlayersRest ++ [Player]
    end,

    % Checks if the end of the game is reached.
    case length(grid:get_open_spots(GridNew)) of
        0 ->
            [P ! finished || P <- Players];
        _ ->
            Player ! {move, self(), GridNew}
    end,

    {reply, {ok, Score}, {GridNew, NewPlayers}};

% Used for testing.
handle_call(state, _From, State) ->
    {reply, {ok, State}, State};
handle_call({setWalls, Walls}, _From, {{W, H, _}, Players}) ->
    {reply, ok, {{W, H, Walls}, Players}}.

% Required for gen_server behaviour.
% Normally you would implement this too,
% but not required for this assignment.
handle_cast(_, State) ->
    {reply, not_implemented, State}.
