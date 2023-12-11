module AppState where

import BoardClass
import Cell
import Data.Set (delete, insert, member)

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

updatePosition :: (BoardClass board) =>
    Integer -- | x coordinate
 -> Integer -- | y coordinate
 -> AppState board
 -> AppState board

updatePosition x y state = state {stateX=x, stateY=y}

movePosition :: (BoardClass board) =>
    Integer -- | delta x
 -> Integer -- | delta y
 -> AppState board
 -> AppState board

movePosition dx dy state@(AppState {stateX=x, stateY=y}) =
    state {stateX=x + dx, stateY=y + dy}

toggleCell :: (BoardClass board) => Cell -> AppState board -> AppState board
toggleCell cell state@(AppState {stateBoard=board}) = do
    let cells = getCells board
    if cell `member` cells then
        state {stateBoard=setCells board $ delete cell cells}
    else
        state {stateBoard=setCells board $ insert cell cells}