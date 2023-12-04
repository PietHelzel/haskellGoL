module GameLogic.GameLogic where

import GameLogic.Cell

class BoardClass a where
    updateBoard :: a -> a

newtype Board = Board [Cell]

update :: Board -> Board
update board = board

instance BoardClass Board where
    updateBoard = update
