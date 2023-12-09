module Main where

import UI (runApp)
import AppState (AppState(..))

import Board (Board(Board))

import Data.Set (fromList)

import Cell
import BoardClass

main :: IO (AppState Board)
main = do
    runApp AppState {statePaused=False, stateBoard=
                Board $ fromList [
                    Cell {x = 0, y = 0},
                    Cell {x = 0, y = 1},
                    Cell {x = (-1), y = 1},
                    Cell {x = 0, y = 2},
                    Cell {x = 1, y = 2}
                ],
            stateRuleset=getDefaultRules,
            stateX = -50,
            stateY = -50,
            stateWidth = 100,
            stateHeight = 50
            }
