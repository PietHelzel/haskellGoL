module Main where

import UI (runApp)
import AppState (AppState(..))

import Board (Board(Board))

import Data.Set (fromList)

import Cell
import BoardClass

import Options.Applicative

data Config = Config
    { boardFilePath :: String
    , ruleFilePath  :: String
    } deriving Show

configParser :: Parser Config
configParser = Config
    <$> option str 
        (long "board"
        <> metavar "path-to-board-file"
        <> value ""
        <> help "The filepath to the board file.")
    <*> option str
        (long "rules"
        <> metavar "path-to-rules-file"
        <> value ""
        <> help "The filepath to the rules file.")


main :: IO (AppState Board)
main = do
    config <- execParser opts
    runApp AppState {statePaused=False, stateBoard=
                Board $ fromList [
                    Cell {x = 0, y = 0},
                    Cell {x = 0, y = 1},
                    Cell {x = (-1), y = 1},
                    Cell {x = 0, y = 2},
                    Cell {x = 1, y = 2}
                ],
            stateRuleset=getDefaultRules,
            stateX = -10,
            stateY = -10,
            stateWidth = 100,
            stateHeight = 50,
            stateTicksBetweenUpdates = 5,
            stateTicks = 0
            }
        where 
        opts = info (configParser <**> helper)
            (fullDesc
            <> progDesc "Haskell Game of Life"
            <> header "Header here :3")
