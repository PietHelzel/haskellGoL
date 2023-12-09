module AppState where

import BoardClass

data (BoardClass board) => AppState board = AppState {
        stateBoard :: board,
        stateRuleset :: RuleSet,
        statePaused :: Bool,
        stateX :: Integer,
        stateY :: Integer,
        stateWidth :: Integer,
        stateHeight :: Integer
    }

togglePaused :: (BoardClass board) => AppState board -> AppState board
togglePaused state@(AppState{statePaused = paused}) = state {statePaused = not paused}

updateBoard :: (BoardClass board) => AppState board -> AppState board
updateBoard state@(AppState{stateBoard = board, stateRuleset = ruleset})
    = state {stateBoard = update board ruleset}

updateExtents :: (BoardClass board) =>
    Integer -- | x coordinate
 -> Integer -- | y coordinate
 -> Integer -- | width
 -> Integer -- | height
 -> AppState board
 -> AppState board

updateExtents
    x y width height state =
        state {stateX=x, stateY=y, stateWidth=width, stateHeight=height}

updateSize :: (BoardClass board) =>
    Integer -- | width
 -> Integer -- | height
 -> AppState board
 -> AppState board

updateSize width height state = state {stateWidth=width, stateHeight=height}