module Main where

import UI (runApp)
import AppState (AppState(..))

import Board (Board(Board))

import Data.Set (fromList)

import BoardClass

import Options.Applicative

import Control.Exception (catch, IOException)

data Config = Config
    { boardFilePath :: String
    , rules         :: String
    } deriving Show

configParser :: Parser Config
configParser = Config
    <$> option str 
        (long "board-file"
        <> metavar "path-to-board-file"
        <> value ""
        <> help "The filepath to the board file.")
    <*> option str
        (long "rules"
        <> metavar "Definition of rules"
        <> value ""
        <> help "Rules in common GoL Notation. . Example: \"23/3\" for the default ruleset.")

getBoardFromConfig :: Config -> IO (Board)
getBoardFromConfig config = getBoard $ boardFilePath config

-- | Gets a board from file and parses it, if a board file is specified
getBoard :: String -> IO (Board)
getBoard "" = return $ Board $ fromList []
getBoard path = do
    contents <- catch (readFile path) handleIOException
    return $ fromString contents

-- | retrieves the rules from a file
getRuleSetFromConfig :: Config -> IO (RuleSet)
getRuleSetFromConfig config = getRules (ruleSetFromString $ rules config)


getRules :: Maybe RuleSet -> IO (RuleSet)
getRules (Just ruleset) = return ruleset
getRules Nothing = error "The given ruleset is malformed."

-- | Handles exceptions reading the board or rule file
handleIOException :: IOException -> IO String
handleIOException e = error $ "An error occured while trying to read the board file: " ++ show e


main :: IO (AppState Board)
main = do
    config <- execParser opts
    --error $ show config
    board <- getBoardFromConfig config
    rules <- getRuleSetFromConfig config
    
    runApp AppState {statePaused=False, 
            stateBoard=board,
            stateRuleset=rules,
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
