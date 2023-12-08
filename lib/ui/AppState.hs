module AppState where

import BoardClass

data (BoardClass board) => AppState board = AppState {
        stateBoard :: board,
        statePaused :: Bool
    }

togglePaused :: (BoardClass board) => AppState board -> AppState board
togglePaused state@(AppState{statePaused = statePaused}) = state {statePaused = not statePaused}
