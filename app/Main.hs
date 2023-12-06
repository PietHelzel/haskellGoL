module Main where

import GameLogic.Board (Board(Board), update)
import GameLogic.Cell (Cell(..))

main :: IO ()
main = do
    let b = Board []
    let c = Cell {x=2, y=2}
    putStrLn "Abc"
