%   David Angulo Velez
%   UvaID - 14977524
%   Bachelor Informatica

%   Het volgende programma implementeerd de basis voor het manipuleren van
%   een rooster voor het spel 'kamertje verhuur'.


-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2,
    add_wall/2, get_cell_walls/2, get_all_walls/2, get_open_spots/1, choose_random_wall/1,
    test_grid/0, print_list/1, check_cell_filled/2]).


new(Width, Height) -> {Width, Height, []}.

test_grid() -> {2, 2, [get_wall(0, 0, north), get_wall(1, 0, east)]}.

get_wall(Width, Height, Dir) ->
    Origin = {Width, Height},
    case Dir of
        north -> {{Width, Height - 1}, Origin};
        east -> {Origin, {Width + 1, Height}};
        south -> {Origin, {Width, Height + 1}};
        west -> {{Width - 1, Height}, Origin};
        _ -> io:format("Use north, east, south or west as an direction")
    end.


has_wall(Wall, Grid) ->
    {_, _, Walls} = Grid,
    lists:member(Wall, Walls).


add_wall(Wall, Grid) ->
    case has_wall(Wall, Grid) of
        false ->
            {Width, Height, Walls} = Grid,
            NewWalls = Walls ++ [Wall],
            {Width, Height, NewWalls};
        true ->
            Grid
    end.

print_list([]) -> "";
print_list([H | T]) -> H ++ print_list(T).

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
    print_list(StringList ++ ["+~n"]).


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
    print_list(StringList ++ ["~n"]).


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


get_cell_walls(X, Y) ->
    [get_wall(X, Y, Dir) || Dir <- [north, east, south, west]].

check_cell_filled({X, Y}, Grid) ->
    {_, _, Walls} = Grid,
    CellWalls = [CellWall || CellWall <- get_cell_walls(X, Y), lists:member(CellWall, Walls)],
    case length(CellWalls) of
        4 ->
            true;
        _ ->
            false
    end.

% Verwijdert duplicates uit een lijst.
remove_duplicates([]) -> [];
remove_duplicates([Head | Tail]) -> [Head | [X || X <- remove_duplicates(Tail), X /= Head]].

% Returned alle walls in het gehele grid.
get_all_walls(Width, Height) ->
    Get_Walls = fun(Get_Walls, Acc, X, Y) ->
        if
            Y >= Height -> % Einde bereikt
                Acc;

            X < Width -> % Zolang regel nog niet afgelopen is
                Acc1 = Acc ++ get_cell_walls(X, Y),
                Get_Walls(Get_Walls, Acc1, X + 1, Y);

            true -> % Ga naar volgende regel
                Get_Walls(Get_Walls, Acc, 0, Y + 1)
            end
        end,

    L = Get_Walls(Get_Walls, [], 0, 0),
    remove_duplicates(L).


get_open_spots({Width, Height, Walls}) ->
    All_Walls = get_all_walls(Width, Height),
    All_Walls -- Walls.


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
