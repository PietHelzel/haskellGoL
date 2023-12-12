-- | This module provides a function to render a board as a String.
module BoardRenderer (renderBoard) where

import BoardClass (BoardClass, getCellsRect)
import Cell

import Data.Set (member)
import Data.List (intercalate)

-- | Render a board into a rectangular String representation with a given size and scroll position.
renderBoard :: (BoardClass board) =>
    -- | The x coordinate
    Integer
    -- | The y coordinate
 -> Integer
    -- | The width
 -> Integer
    -- | The height
 -> Integer
    -- | The board to use
 -> board
 -> String

renderBoard x y width height board = do
    let cells = getCellsRect x y width height board
    let s = [
            if Cell{x=x', y=y'} `member` cells then
                '█'
            else
                ' '
            | y' <- [y..y+height-1], x' <- [x..x+width-1]]
    intercalate "\n" $ chunksOf (fromInteger width) s

-- | Splits a list into chunks of a specified size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf 0 _ = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)
