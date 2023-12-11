module Main where

import UI (runApp)
import AppState (AppState(..))

import Board (Board(Board))

import Data.Set (fromList)

--import Cell
import BoardClass

import Options.Applicative

import Control.Exception (catch, IOException)

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

-- | Gets a board from file and parses it, if a board file is specified
getBoard :: String -> IO (Board)
getBoard "" = return $ Board $ fromList []
getBoard path = do
    contents <- catch (readFile path) handleIOException
    return $ fromString contents

-- | Handles exceptions reading the board or rule file
handleIOException :: IOException -> IO String
handleIOException e = error $ "An error occured while trying to read the board or rule file: " ++ show e


main :: IO (AppState Board)
main = do
    config <- execParser opts
    board <- getBoard (boardFilePath config)
    runApp AppState {statePaused=False, 
            stateBoard=board,
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
