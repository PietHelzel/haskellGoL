module GameLogic.Cell where

data Cell = Cell {x::Integer, y::Integer} deriving (Eq, Show, Ord)
