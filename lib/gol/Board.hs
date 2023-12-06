-- | This module provides a basic implementation of the Game of Life algorithm.
-- It implements the 'BoardClass' class to be usable in a generic manner.
module Board (Board(Board)) where

import Data.List (nub)
import Data.Set (fromList)

import Cell
import BoardClass

-- | The board data type. It contains a list of 'Cell', representing the currently alive cells.
data Board = Board [Cell] deriving (Show)

-- | Updates the board using a ruleset.
updateBoard :: Board -> RuleSet -> Board
updateBoard board rules = Board [cell | cell <- cells, willExist board rules cell]
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

-- | Gets all the cells that are currently alive as well as their neighbours.
-- This is required because a dead cell can be born in the next generation given it has the
-- proper number of neighbours.
getAllImportantCells :: Board -> [Cell]
getAllImportantCells (Board cells) = nub $ concat [cells, concat [getNeighbours c | c <- cells]]

-- | Gets all neighbouring cells of a specific cell.
getNeighbours :: Cell -> [Cell]
getNeighbours c = [moveCell dx dy c | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- | Simply returns all alive cells.
getBoardCells :: Board -> [Cell]
getBoardCells (Board cells) = cells

-- | Compares two 'Board' by converting the list of cells to a set, then comparing the sets.
boardEquals :: Board -> Board -> Bool
boardEquals (Board c1) (Board c2) = fromList c1 == fromList c2

instance BoardClass Board where
    update = updateBoard
    getCells = getBoardCells

instance Eq Board where
    (==) = boardEquals
