-- | This modules contains a class to make boards more generic as well as
-- a data type for a ruleset.
module BoardClass where

import Cell
import Data.Set

-- | This class specifies a generic interface for a board.
-- It allows the concrete board implementation to be replaced without other changes to the code.
class BoardClass a where
    -- | Updates the board using a RuleSet, returning a new board.
    update :: a -> RuleSet -> a
    -- | Gets a set of all living cells.
    getCells :: a -> Set Cell
    -- | Gets a set of living cells within a rectangle area.
    -- This makes rendering possible while only getting necessary cells.
    getCellsRect ::
        Integer -- | The x coordinate of the bottom left corner.
     -> Integer -- | The y coordinate of the bottom left corner.
     -> Integer -- | The width of the region.
     -> Integer -- | The height of the region.
     -> a       -- | The board.
     -> Set Cell

-- | A RuleSet specifies the number of neighbours a cell needs to have in order
-- to survive a generation (for a living cell) or to get born in the next one (for a dead cell).
data RuleSet = RuleSet {survive::[Int], birth::[Int]}

-- | This function returns the default ruleset used for the Game of Life.
getDefaultRules :: RuleSet
getDefaultRules = RuleSet {survive = [2, 3], birth = [3]}
