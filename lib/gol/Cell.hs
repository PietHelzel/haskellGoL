-- | This module contains the data type Cell, which represents a position
-- on a board.
module Cell where

-- | A data type containing x and y coordinates.
data Cell = Cell {x::Integer, y::Integer} deriving (Eq, Show, Ord)
