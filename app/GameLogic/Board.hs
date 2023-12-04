module GameLogic.Board where

import GameLogic.Cell
import GameLogic.BoardClass

data Board = Board [Cell]

update :: Board -> RuleSet -> Board
update board rules = board

newBoard :: Board
newBoard = Board []

instance BoardClass Board where
    updateBoard = update