-- | This module provides a basic implementation of the Game of Life algorithm.
-- It implements the 'BoardClass' class to be usable in a generic manner.
module Board where

import Data.List (nub)
import Data.Set (fromList)

import Cell
import BoardClass

-- | The board data type. It contains a list of 'Cell', representing the currently alive cells.
data Board = Board [Cell] deriving (Show)

-- | Updates the board using a ruleset.
update :: Board -> RuleSet -> Board
update board rules = Board $ nub [cell | cell <- cells, willExist board rules cell]
    where cells = getAllImportantCells board

-- | Checks if a cell will exist in the next generation.
willExist :: Board -> RuleSet -> Cell -> Bool
willExist board@(Board cells) (RuleSet {survive=survive, birth=birth}) c
    | neighbours `elem` birth = True
    | neighbours `elem` survive = c `elem` cells
    | otherwise = False
    where neighbours = countNeighbours board c

-- | Counts the number of living neighbours a cell has within a board.
countNeighbours :: Board -> Cell -> Integer
countNeighbours (Board cells) c = sum [1 | n <- getNeighbours c, n `elem` cells]

-- TODO: Benchmark weil interessant :D (dedup vs. nicht dedup)
-- | Gets all the cells that are currently alive as well as their neighbours.
-- This is required because a dead cell can be born in the next generation given it has the
-- proper number of neighbours.
getAllImportantCells :: Board -> [Cell]
getAllImportantCells (Board cells) = concat [cells, concat [getNeighbours c | c <- cells]]

-- TODO: Make shorter with list comprehension
-- | Gets all neighbouring cells of a specific cell.
getNeighbours :: Cell -> [Cell]
getNeighbours c = [
        Cell {x=x c - 1, y = y c - 1},
        Cell {x=x c, y = y c - 1},
        Cell {x=x c + 1, y = y c - 1},
        Cell {x=x c - 1, y = y c},
        Cell {x=x c + 1, y = y c},
        Cell {x=x c - 1, y = y c + 1},
        Cell {x=x c, y = y c + 1},
        Cell {x=x c + 1, y = y c + 1}
    ]

boardEquals :: Board -> Board -> Bool
boardEquals (Board c1) (Board c2) = fromList c1 == fromList c2

instance BoardClass Board where
    updateBoard = update

instance Eq Board where
    (==) = boardEquals
