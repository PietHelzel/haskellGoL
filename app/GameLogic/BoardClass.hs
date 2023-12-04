module GameLogic.BoardClass where

class BoardClass a where
    updateBoard :: a -> RuleSet -> a

data RuleSet = RuleSet {survive::[Integer], birth::[Integer]}

getDefaultRules :: RuleSet
getDefaultRules = RuleSet {survive = [2, 3], birth = [3]}