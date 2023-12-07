-- | This module provides a basic implementation of the Game of Life algorithm.
-- It implements the 'BoardClass' class to be usable in a generic manner.
module Board (Board(Board)) where

import Data.Set as DS (Set, fromList, member, unions, union, filter, map)

import Cell
import BoardClass

-- | The board data type. It contains a set of 'Cell', representing the currently alive cells.
data Board = Board (Set Cell) deriving (Show, Eq)

-- | Updates the board using a ruleset.
updateBoard :: Board -> RuleSet -> Board
updateBoard board rules = Board $ DS.filter (willExist board rules) cells
    where cells = getAllImportantCells board

-- | Checks if a cell will exist in the next generation.
willExist :: Board -> RuleSet -> Cell -> Bool
willExist board@(Board cells) (RuleSet {survive=survive, birth=birth}) c
    | neighbours `elem` birth = True
    | neighbours `elem` survive = c `member` cells
    | otherwise = False
    where neighbours = countNeighbours board c

-- | Counts the number of living neighbours a cell has within a board.
countNeighbours :: Board -> Cell -> Int
countNeighbours (Board cells) c = length $ DS.filter (`member` cells) (getNeighbours c)

-- | Gets all the cells that are currently alive as well as their neighbours.
-- This is required because a dead cell can be born in the next generation given it has the
-- proper number of neighbours.
getAllImportantCells :: Board -> Set Cell
getAllImportantCells (Board cells) = union cells $ unions $ DS.map getNeighbours cells

-- | Gets all neighbouring cells of a specific cell.
getNeighbours :: Cell -> Set Cell
getNeighbours c = fromList [moveCell dx dy c | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

-- | Simply returns all alive cells.
getBoardCells :: Board -> Set Cell
getBoardCells (Board cells) = cells

instance BoardClass Board where
    update = updateBoard
    getCells = getBoardCells
