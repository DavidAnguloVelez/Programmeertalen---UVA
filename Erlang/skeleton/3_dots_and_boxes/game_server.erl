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


% TODO: You need to inform the first player to move.
init({Width, Height, Players}) ->
    Grid = grid:new(Width, Height),
    % case length(Players) of
    %     0 ->
    %         {error, "Not enough players"};
    %     _ ->
            [Player | _] = Players,
    % end,

    Player ! {move, self(), Grid},
    {ok, {Grid, Players}}.


calculate_score(Wall, Grid) ->
    {Cell1, Cell2} = Wall,

    % Check of eerste cell ingebouwd is.
    case grid:check_cell_filled(Cell1, Grid) of
        true ->
            Score = 1;

        false ->
            Score = 0
    end,

    % Check of tweede cell ingebouwd is.
    case grid:check_cell_filled(Cell2, Grid) of
        true ->
            Score1 = Score + 1;

        false ->
            Score1 = Score
    end,


    Score1.

handle_call({move, Wall}, _From, {Grid, Players}) ->
    GridNew = grid:add_wall(Wall, Grid),
    [Player | PlayersRest] = Players,

    Score = calculate_score(Wall, GridNew),

    % Check of speler een punt heeft gehaald.
    if
        Score > 0 ->
            NewPlayers = [Player] ++ PlayersRest;

        true ->
            NewPlayers = PlayersRest ++ [Player]
    end,

    % Check of einde van het spel is bereikt.
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
