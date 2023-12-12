-- | This modules contains a class to make boards more generic as well as
-- a data type for a ruleset.
module BoardClass where

import Cell
import Data.Set

import Data.List.Split (splitOn)

import Text.Regex.TDFA

-- | This class specifies a generic interface for a board.
-- It allows the concrete board implementation to be replaced without other changes to the code.
class BoardClass a where
    -- | Updates the board using a RuleSet, returning a new board.
    update :: a -> RuleSet -> a
    -- | Gets a set of all living cells.
    getCells :: a -> Set Cell
    -- | Returns a new board with the cells modified.
    setCells :: a -> Set Cell -> a
    -- | Gets a set of living cells within a rectangle area.
    -- This makes rendering possible while only getting necessary cells.
    getCellsRect ::
        -- | The x coordinate of the top left corner.
        Integer
        -- | The y coordinate of the top left corner.
     -> Integer
     -- | The width of the region.
     -> Integer
     -- | The height of the region.
     -> Integer
     -- | The board.
     -> a
     -> Set Cell
    -- | Converts a string representation into a board
    fromString :: String -> Maybe a
    -- | Converts the board to a string representation
    toString :: a -> String

-- | A RuleSet specifies the number of neighbours a cell needs to have in order
-- to survive a generation (for a living cell) or to get born in the next one (for a dead cell).
data RuleSet = RuleSet {survive::[Int], birth::[Int]}

-- | This function returns the default ruleset used for the Game of Life.
getDefaultRules :: RuleSet
getDefaultRules = RuleSet {survive = [2, 3], birth = [3]}

matchesRuleSetNotation :: String -> Bool
matchesRuleSetNotation s = s =~ "^[0-9]+/[0-9]+$"

-- | Converts a string into a RuleSet. For notation see README file.
ruleSetFromString :: String -> Maybe RuleSet
ruleSetFromString s | matchesRuleSetNotation s = Just $ ruleSetFromStringHelper s
                    | otherwise = Nothing

ruleSetFromStringHelper :: String -> RuleSet
ruleSetFromStringHelper s = do
    let sections = splitOn "/" s
    let survive = sections !! 0
    let birth = sections !! 1
    RuleSet {survive=(Prelude.map (\c -> read [c]) survive), birth=((Prelude.map) (\c -> read [c]) birth)}

