{-# LANGUAGE LambdaCase #-}
-- | This module provides a basic implementation of the Game of Life algorithm.
-- It implements the 'BoardClass' class to be usable in a generic manner.
module Board (Board(Board)) where

import Data.Set as DS (Set, fromList, member, unions, union, filter, map, toList)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Text.Regex.TDFA

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

-- | Returns all cells within a rectangle region.
getBoardCellsRect ::
    -- | The x coordinate of the top left corner.
    Integer
    -- | The y coordinate of the top left corner.
 -> Integer
    -- | The width of the region.
 -> Integer
    -- | The height of the region.
 -> Integer
    -- | The board.
 -> Board
 -> Set Cell
getBoardCellsRect x y width height board = DS.filter (
        \c -> Cell.x c >= x && Cell.y c >= y && Cell.x c < x + width && Cell.y c < y + height
    ) $ getBoardCells board

-- | Sets the cells of the board to a new set.
setBoardCells :: Board -> Set Cell -> Board
setBoardCells _ cells = Board cells

-- | Converts the board to a string. Consult the README for more information.
boardToString :: Board -> String
boardToString (Board cells) = intercalate "\n" $ DS.toList $ DS.map (\c -> (show $ x c) ++ "," ++ (show $ y c)) cells

lineMatchesNotation :: String -> Bool
lineMatchesNotation s = s =~ "^(-?[0-9]+,-?[0-9]+)$"

-- | Converts a String to a board object. Consult the README for more information.
-- boardFromString :: String -> Maybe Board
-- boardFromString s | boardMatchesNotation s = Just $ boardFromStringHelper s
--                   | otherwise = Nothing

boardFromString :: String -> Board
boardFromString s = do
    Board $ fromList $ Prelude.map (\l -> do
            let segments = splitOn "," l
            let x' = read $ segments !! 0
            let y' = read $ segments !! 1
            Cell {x=x', y=y'}
        ) $ Prelude.filter lineMatchesNotation $ lines s


instance BoardClass Board where
    update = updateBoard
    getCells = getBoardCells
    getCellsRect = getBoardCellsRect
    setCells = setBoardCells
    toString = boardToString
    fromString = Just . boardFromString
