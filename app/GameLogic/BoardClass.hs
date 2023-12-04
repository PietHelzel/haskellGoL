module GameLogic.BoardClass where

class BoardClass a where
    updateBoard :: a -> RuleSet -> a

data RuleSet = RuleSet {death::[Integer], birth::[Integer]}