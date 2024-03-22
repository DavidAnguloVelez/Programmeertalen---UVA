%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis voor het manipuleren van
%   een rooster voor het spel 'kamertje verhuur'.


-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2,
    add_wall/2, get_cell_walls/2, get_all_walls/2, get_open_spots/1, choose_random_wall/1,
    test_grid/0, list_to_string/1, check_cell_filled/2, get_open_cell_walls/3,
    get_completable_walls/1, concatenate_lists_of_list/1]).

% Returns a new grid with the given Width and Height and no walls.
new(Width, Height) -> {Width, Height, []}.

% Returns a grid that makes it easier to test
test_grid() -> {2, 2, [get_wall(0, 0, north), get_wall(1, 0, east), get_wall(0, 0, west),
                        get_wall(0, 0, east)]}.

% Returns a wall of the given cell in the given direction.
get_wall(Width, Height, Dir) ->
    Origin = {Width, Height},
    case Dir of
        north -> {{Width, Height - 1}, Origin};
        east -> {Origin, {Width + 1, Height}};
        south -> {Origin, {Width, Height + 1}};
        west -> {{Width - 1, Height}, Origin};
        _ -> io:format("Use north, east, south or west as an direction")
    end.

% Returns a boolean depending on if a grid contains the given wall.
has_wall(Wall, Grid) ->
    {_, _, Walls} = Grid,
    lists:member(Wall, Walls).

% Returns a new grid with the given wall added.
add_wall(Wall, Grid) ->
    case has_wall(Wall, Grid) of
        false ->
            {Width, Height, Walls} = Grid,
            NewWalls = Walls ++ [Wall],
            {Width, Height, NewWalls};
        true ->
            Grid
    end.

% Converts a list of strings into a single string.
list_to_string([]) -> "";
list_to_string([H | T]) -> H ++ list_to_string(T).

% Prints all the hlines of a given row and grid (a hline exists from all the
% vertical walls from that row).
show_hlines(Row, {Width, _, Walls}) ->
    RowWalls = [Sx|| {{Sx, Sy}, {_, Ey}} <- Walls, Sy == Row - 1, Ey == Row],
    RowCoords = lists:seq(0, Width - 1),
    StringList = lists:map(
        fun(X) ->
            case lists:member(X, RowWalls) of
                true ->
                    "+--";

                false ->
                    "+  "
            end
        end, RowCoords),
    list_to_string(StringList ++ ["+~n"]).

% Prints all the vlines of a given row and grid (a vline exists from all the
% horizontal walls from that row).
show_vlines(Row, {Width, _, Walls}) ->
    RowWalls = [Sx || {{Sx, Sy}, {_, Ey}} <- Walls, Sy == Row, Ey == Row],
    RowCoords = lists:seq(0, Width),
    StringList = lists:map(
        fun(X) ->
            case lists:member(X - 1, RowWalls) of
                true when X == Width ->
                    "|";

                true ->
                    "|  ";

                false when X == Width ->
                    " ";

                false ->
                    "   "
            end
        end, RowCoords),
    list_to_string(StringList ++ ["~n"]).


% Prints this grid in a structured format
% using the show_Xlines functions.
print(Grid) ->
    {_, H, _} = Grid,
    lists:map(fun(Row) ->
        io:fwrite(show_hlines(Row, Grid)),

        case Row < H of
            true ->
                io:fwrite(show_vlines(Row, Grid));
            false ->
                ok
        end
    end, lists:seq(0, H)),
    io:fwrite("~n"),
    ok.

% Returns all the walls of a given cell.
get_cell_walls(X, Y) ->
    [get_wall(X, Y, Dir) || Dir <- [east, south, north, west]].

% Returns a boolean depending if a cell has all walls filled or not.
check_cell_filled({X, Y}, Grid) ->
    {_, _, Walls} = Grid,
    CellWalls = [CellWall || CellWall <- get_cell_walls(X, Y), lists:member(CellWall, Walls)],
    case length(CellWalls) of
        4 ->
            true;
        _ ->
            false
    end.

% Concatenates a list of lists into a single lists with all elements from all the lists.
concatenate_lists_of_list([]) -> [];
concatenate_lists_of_list([[HH| TH] | T]) -> [HH | TH] ++ concatenate_lists_of_list(T);
concatenate_lists_of_list(_) -> {error, "Not a list of lists"}.

% Removes all duplicates from a given list.
remove_duplicates([]) -> [];
remove_duplicates([Head | Tail]) -> [Head | [X || X <- remove_duplicates(Tail), X /= Head]].

% Returns all the walls of the entire grid.
get_all_walls(Width, Height) ->
    CoordsX = lists:seq(0, Width - 1),
    CoordsY = lists:seq(0, Height - 1),
    AllWallsDuped = [get_cell_walls(X, Y) || X <- CoordsX, Y <- CoordsY],
    AllWallsDuped1 = concatenate_lists_of_list(AllWallsDuped),
    remove_duplicates(AllWallsDuped1).

% Returns all opens spots from a grid.
get_open_spots({Width, Height, Walls}) ->
    AllWalls = get_all_walls(Width, Height),
    AllWalls -- Walls.

% Returns a random wall that has not been placed yet from a grid.
choose_random_wall(Grid) ->
    Options = get_open_spots(Grid),
    if
        Options == [] ->
            [];

        true ->
            Length = length(Options),
            Random_Index = rand:uniform(Length),
            lists:nth(Random_Index, Options)
    end.

% Returns all the walls of a cell that have yet to be placed.
get_open_cell_walls(X, Y, Grid) ->
    [CellWall || CellWall <- get_cell_walls(X, Y), not has_wall(CellWall, Grid)].

% Returns all walls from a grid that would completely enclose a cell
% (with a single placement).
get_completable_walls(Grid) ->
    {X, Y, _} = Grid,
    CoordsX = lists:seq(0, X - 1),
    CoordsY = lists:seq(0, Y - 1),
    AllCellCoords = [{XC, YC} || XC <- CoordsX, YC <- CoordsY],
    [Wall || {XC, YC} <- AllCellCoords, Wall <- get_open_cell_walls(XC, YC, Grid),
                                length(get_open_cell_walls(XC, YC, Grid)) == 1].
