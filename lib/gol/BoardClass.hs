-- | This modules contains a class to make boards more generic as well as
-- a data type for a ruleset.
module BoardClass where

-- | This class specifies a generic interface for a board.
-- It allows the concrete board implementation to be replaced without other changes to the code.
class BoardClass a where
    -- | updates the board using a RuleSet, returning a new board.
    updateBoard :: a -> RuleSet -> a

-- | A RuleSet specifies the number of neighbours a cell needs to have in order
-- to survive a generation (for a living cell) or to get born in the next one (for a dead cell).
data RuleSet = RuleSet {survive::[Integer], birth::[Integer]}

-- | This function returns the default ruleset used for the Game of Life.
getDefaultRules :: RuleSet
getDefaultRules = RuleSet {survive = [2, 3], birth = [3]}
