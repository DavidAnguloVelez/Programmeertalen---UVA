{- David Angulo Vélez
   14977524
   Bachelor Informatica - UvA

   This program implements multiple functions to solve a given sudoku. The
   solver has two options, to solve a normal sudoku with the standard rules,
   or to solve it follwing the "NRC sudoku" rules.

   The solver checks if a sudoku is valid, depending on the rules specified.
-}

import System.Environment
import Data.List
import Data.Ord (comparing)
import Data.Data (Constr)

type Row = Int
type Column = Int
type Value = Int
type Grid = [[Value]] -- Only used to read/write from/to a file.
type Sudoku = (Row, Column) -> Value
type Constraint = (Row, Column, [Value])
type Node = (Sudoku, [Constraint])
type Solver = Sudoku -> Sudoku

-- Gives the length of the subgrids in the sudoku.
subgridLength :: Int
subgridLength = 3

positions :: [Int]
positions = [1..9]

values :: [Value]
values = [1..9]

blocks :: [[Int]]
blocks = [[1..3],[4..6],[7..9]]

nrcBlocks :: [[Int]]
nrcBlocks = [[2..4], [6..8]]

centerOfBlocks :: [Int]
centerOfBlocks = [2, 5, 8]

sud2grid :: Sudoku -> Grid
sud2grid s = [[s (r, c) | c <- positions] | r <- positions]

grid2sud :: Grid -> Sudoku
grid2sud gr = \(r, c) -> pos gr (r, c)
  where pos :: [[a]] -> (Row,Column) -> a
        pos gr (r, c) = (gr !! (r - 1)) !! (c - 1)

-- Extends a sudoku with a value at (row, column).
extend :: Sudoku -> (Row, Column, Value) -> Sudoku
extend sud (r, c, v) (i, j) = if r == i && c == j then v else sud (i, j)

-- Read a file-sudoku with a Grid like format into a Sudoku.
readSudoku :: String -> IO Sudoku
readSudoku filename =
    do stringGrid <- readFile filename
       return $ (grid2sud . splitStringIntoGrid) stringGrid
       where splitStringIntoGrid = map (map readint . words) . lines
             readint x = read x :: Int

{- Prints a Sudoku to the terminal by transforming it to a grid first.
   Do not modify this, or your tests will fail.
-}
printSudoku :: Sudoku -> IO ()
printSudoku = putStr . unlines . map (unwords . map show) . sud2grid

-- Helper to parse command-line arguments.
getSudokuName :: [String] -> String
getSudokuName [] = error "Filename of sudoku as first argument."
getSudokuName (x:_) = x

-- Returns the given the free values from the given row.
freeInRow :: Sudoku -> Row -> [Value]
freeInRow s r = values \\ [s (r, c) | c <- positions]

-- Returns the given the free values from the given column.
freeInColumn :: Sudoku -> Column -> [Value]
freeInColumn s c = values \\ [s (r, c) | r <- positions]

{- Returns all coordinates of the subgrid in which the coordinate finds.
   itself in -}
positionsSubgrid :: Sudoku -> (Row, Column) -> [(Row, Column)]
positionsSubgrid s (r, c) = [(r, c) | r <- blockR, c <- blockC]
   where
      blockR = blocks !! ((r - 1) `div` subgridLength)
      blockC = blocks !! ((c - 1) `div` subgridLength)

{- Returns all coordinates of the NRC subgrid in which the coordinate finds.
   itself in -}
positionsSubgridNRC :: Sudoku -> (Row, Column) -> [(Row, Column)]
positionsSubgridNRC s (r, c)
   | not (r `elem` nrcBlockAll && c `elem` nrcBlockAll) = []
   | otherwise = [(r, c) | r <- nrcGridR, c <- nrcGridC]
   where
      nrcGridR = if r <= last (head nrcBlocks) then head nrcBlocks else last nrcBlocks
      nrcGridC = if c <= last (head nrcBlocks) then head nrcBlocks else last nrcBlocks
      nrcBlockAll = [v | nrcBlock <- nrcBlocks, v <- nrcBlock]

{- Returns the free values in a certain subgrid of the sudoku. The subgrid is
   determined by the coordinate given to it (the subgrid that contains the
   given coordinate).
-}
freeInSubgrid :: Sudoku -> (Row, Column) -> [Value]
freeInSubgrid s (r, c) = values \\ [s (r, c) | (r, c) <- positionsSubgrid s (r, c)]

{- Returns the free values in a certain NRC subgrid of the sudoku. The subgrid
   is determined by the coordinate given to it (the NRC subgrid that contains
   the given coordinate).
-}
freeInSubgridNRC :: Sudoku -> (Row, Column) -> [Value]
freeInSubgridNRC s (r, c)
   | not (r `elem` nrcBlockAll && c `elem` nrcBlockAll) = values
   | otherwise = values \\ [s (r, c) | (r, c) <- positionsSubgridNRC s (r, c)]
   where
      nrcBlockAll = [v | nrcBlock <- nrcBlocks, v <- nrcBlock]

{- Checks all possible values on a certain coordinate, including the row,
columb and subgrid. -}
freeAtPos :: Sudoku -> (Row,Column) -> [String] -> [Value]
freeAtPos s (r, c) str
   | "nrc" `elem` str = freeAtPos s (r, c) [] `intersect` freeInSubgridNRC s (r, c)
   | otherwise = freeInRow s r `intersect` freeInColumn s c
                     `intersect` freeInSubgrid s (r, c)

-- Returns all coordinates in the sudoku that are still empty.
openPositions :: Sudoku -> [String] ->[(Row, Column)]
openPositions s str = [(r, c) | r <- positions, c <- positions,
                     not (null (freeAtPos s (r, c) str))]

-- Returns true if a list has duplicates, false if not.
listNoDuplicates :: Eq a => [a] -> Bool
listNoDuplicates xs = length xs == length (nub xs)

-- Checks if a row obeys the rules of sudoku.
rowValid :: Sudoku -> Row -> Bool
rowValid s r = listNoDuplicates [s (r, c) | c <- positions, s (r, c) /= 0]

-- Checks if a column obeys the rules of sudoku.
columnValid :: Sudoku -> Column -> Bool
columnValid s c = listNoDuplicates [s (r, c) | r <- positions, s (r, c) /= 0]

-- Checks if a subgrid obeys the rules of sudoku.
subgridValid :: Sudoku -> (Row, Column) -> Bool
subgridValid s (r, c) = listNoDuplicates values
   where
      values = [s (r, c) | (r, c) <- positionsSubgrid s (r, c), s (r, c) /= 0]

-- Checks if a NRC subgrid is valid.
subgridValidNRC :: Sudoku -> (Row, Column) -> Bool
subgridValidNRC s (r, c) = listNoDuplicates [s (r, c) |
                                             (r, c) <- positionsSubgridNRC s (r, c),
                                             s (r, c) /= 0]

-- Checks if a the whole sudoku obeys the rules of sudoku.
consistent :: Sudoku -> [String] -> Bool
consistent s str
   | "nrc" `elem` str = and (rowsCheck ++ columnsCheck ++ subgridsCheck ++ subgridNRCCheck)
   | otherwise = and (rowsCheck ++ columnsCheck ++ subgridsCheck)
   where
      rowsCheck = [rowValid s r | r <- positions]
      columnsCheck = [columnValid s c | c <- positions]
      subgridsCheck = [subgridValid s (r, c) | r <- centerOfBlocks,
                                                c <- centerOfBlocks]
      subgridNRCCheck = [subgridValidNRC s (r, c) | r <- centerOfBlocks, c <- centerOfBlocks]

-- Solving ---------------------------------------------------------------------

-- Prints a node.
printNode :: Node -> IO()
printNode = printSudoku . fst

-- Returns list of all constraints of a sudoku sorted on total options.
constraints :: Sudoku -> [String] -> [Constraint]
constraints s str = sortBy (comparing (\(_, _, vs) -> length vs)) constraints
      where
         constraints = [(r, c, freeAtPos s (r, c) str) | r <- positions,
                                                         c <- positions,
                                                         s (r, c) == 0]

{- Solves the given sudoku, if the given list of strings contains "nrc",
      the sudoku will get solved with the special properties of the
      NRC sudoku. -}
solver :: Sudoku -> [String] -> Sudoku
solver s str = solver' [s]
   where
   solver' :: [Sudoku] -> Sudoku
   solver' (s:ss)
      | not (consistent s str) = error "Unsolvable"
      | null const = s
      | otherwise = solver' ([extend s (r, c, v) | v <- vs] ++ ss)
         where
            const = constraints s str
            (r, c, vs) = head const

main :: IO ()
main =
   do args <- getArgs
      sud <- (readSudoku . getSudokuName) args
      printSudoku (solver sud args)
