module BoardRenderer (renderBoard) where

import BoardClass (BoardClass, getCellsRect)
import Cell

import Data.Set (member)
import Data.List (intercalate)

renderBoard :: (BoardClass board) =>
    Integer -- | The x coordinate
 -> Integer -- | The y coordinate
 -> Integer -- | The width
 -> Integer -- | The height
 -> board   -- | The board to use
 -> String

renderBoard x y width height board = do
    let cells = getCellsRect x y width height board
    let s = [
            if Cell{x=x', y=y'} `member` cells then
                '#'
            else
                '.'
            | y' <- [y..y+height-1], x' <- [x..x+width-1]]
    intercalate "\n" $ chunksOf (fromInteger width) s

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 _ = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
