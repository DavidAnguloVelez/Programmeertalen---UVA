-module(grid).
-export([show_hlines/2, show_vlines/2, print/1, new/2, get_wall/3, has_wall/2, add_wall/2, get_all_walls/2]).

new(Width, Height) -> {Width, Height, []}.

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

show_hlines(Row, {Width, _, Walls}) ->
    RowWalls = [{{Sx, Sy}, {Ex, Ey}} || {{Sx, Sy}, {Ex, Ey}} <- Walls, Sy == Row - 1, Ey == Row],
    SortedRowWalls = lists:sort(fun({{Sx1, _}, _}, {{Sx2, _}, _}) -> Sx1 < Sx2 end, RowWalls),

    PrintWalls = fun(PrintWalls, Acc, I, RemWalls) ->
        case RemWalls of
            _ when I >= Width ->
                Acc ++ "+~n";

            [] ->
                Acc1 = Acc ++ "+  ",
                PrintWalls(PrintWalls, Acc1, I + 1, RemWalls);

            _ ->
                [Wall | RemWallsRest] = RemWalls,
                {{Sx, _}, _} = Wall,
                if
                    I == Sx ->
                        Acc1 = Acc ++ "+--",
                        PrintWalls(PrintWalls, Acc1, I + 1, RemWallsRest);
                    I /= Sx ->
                        Acc1 = Acc ++ "+  ",
                        PrintWalls(PrintWalls, Acc1, I + 1, [Wall | RemWallsRest])
                end
        end
    end,

    PrintWalls(PrintWalls, "", 0, SortedRowWalls).

show_vlines(Row, {Width, _, Walls}) ->
    RowWalls = [{{Sx, Sy}, {Ex, Ey}} || {{Sx, Sy}, {Ex, Ey}} <- Walls, Sy == Row, Sy == Ey],
    SortedRowWalls = lists:sort(fun({{Sx1, _}, _}, {{Sx2, _}, _}) -> Sx1 < Sx2 end, RowWalls),

    PrintWalls = fun(PrintWalls, Acc, I, RemWalls) ->
        case RemWalls of
            _ when I >= Width + 1 ->
                % io:format("Case width reached~n~n"),
                Acc ++ "~n";

            [] ->
                % io:format("Case empty list~n~n"),
                Acc ++ " ~n";

            _ ->
                [Wall | RemWallsRest] = RemWalls,
                {{Sx, _}, _} = Wall,
                if
                    Sx == I - 1 ->
                        % io:format("Case wall found~n Wall: ~p~n~n", [Wall]),
                        Acc1 = Acc ++ "|  ",
                        PrintWalls(PrintWalls, Acc1, I + 1, RemWallsRest);

                    true->
                        %  io:format("Case no wall found~n Wall: ~p~n~n", [Wall]),
                        Acc1 = Acc ++ "   ",
                        PrintWalls(PrintWalls, Acc1, I + 1, RemWalls)
                end
        end
    end,

    PrintWalls(PrintWalls, "", 0, SortedRowWalls).

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

get_all_walls(Width, Height) ->
    X_coords = lists:seq(0, Width),
    Y_coords = lists:seq(0, Height),
    Duped_list = [get_cell_walls(X, Y) || X <- X_coords, Y <- Y_coords],
    Duped_list.