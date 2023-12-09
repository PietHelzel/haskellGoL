module AppState where

import BoardClass

data (BoardClass board) => AppState board = AppState {
        stateBoard :: board,
        stateRuleset :: RuleSet,
        statePaused :: Bool
    }

togglePaused :: (BoardClass board) => AppState board -> AppState board
togglePaused state@(AppState{statePaused = statePaused}) = state {statePaused = not statePaused}

updateBoard :: (BoardClass board) => AppState board -> AppState board
updateBoard state@(AppState{stateBoard = board, stateRuleset = ruleset})
    = state {stateBoard = update board ruleset}