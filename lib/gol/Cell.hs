-- | This module contains the data type Cell, which represents a position
-- on a board.
module Cell where

-- | A data type containing x and y coordinates.
data Cell = Cell {x::Integer, y::Integer} deriving (Eq, Show, Ord)

-- | Moves a cell by a given x and y offset.
moveCell ::
    Integer -> -- | The x offset.
    Integer -> -- | The y offset.
    Cell ->
    Cell
moveCell dx dy Cell{x = x, y = y} = Cell {x = x + dx, y = y + dy}
