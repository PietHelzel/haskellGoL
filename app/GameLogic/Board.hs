module GameLogic.Board where

import Data.Set

import GameLogic.Cell
import GameLogic.BoardClass

data Board = Board [Cell] deriving Show

update :: Board -> RuleSet -> Board
update board rules = Board $ toList $ fromList [cell | cell <- cells, willExist board rules cell]
    where cells = getAllImportantCells board

willExist :: Board -> RuleSet -> Cell -> Bool
willExist board@(Board cells) (RuleSet {survive=survive, birth=birth}) c
    | neighbours `elem` birth = True
    | neighbours `elem` survive = c `elem` cells
    | otherwise = False
    where neighbours = countNeighbours board c

countNeighbours :: Board -> Cell -> Integer
countNeighbours (Board cells) c = sum [1 | n <- getNeighbours c, n `elem` cells]

-- TODO: Benchmark weil interessant :D (dedup vs. nicht dedup)
getAllImportantCells :: Board -> [Cell]
getAllImportantCells (Board cells) = concat [cells, concat [getNeighbours c | c <- cells]]

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

instance BoardClass Board where
    updateBoard = update