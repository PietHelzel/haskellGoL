module GameLogic.Board where

import GameLogic.Cell
import GameLogic.BoardClass

data Board = Board [Cell]

update :: Board -> RuleSet -> Board
update board@(Board cells) rules = Board [cell | cell <- cells, willExist board rules cell]

willExist :: Board -> RuleSet -> Cell -> Bool
willExist board@(Board cells) (RuleSet {death=death, birth=birth}) c
    | neighbours `elem` death = False
    | neighbours `elem` birth = True
    | otherwise = c `elem` cells
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