-- | This module provides the 'AppState' data type which represents the current state of the program.
-- Additionally, multiple functions to manipulate the state conveniently are provided.
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
        stateHeight :: Integer,
        stateTicksBetweenUpdates :: Integer, -- | The number of ticks that have to pass before the board is updated.
        stateTicks :: Integer -- | The number of ticks that have passed in total.
    }

-- | Toggles the paused flag.
togglePaused :: (BoardClass board) => AppState board -> AppState board
togglePaused state@(AppState{statePaused = paused}) = state {statePaused = not paused}

-- | Calls the update function on the board.
updateBoard :: (BoardClass board) => AppState board -> AppState board
updateBoard state@(AppState{stateBoard = board, stateRuleset = ruleset})
    = state {stateBoard = update board ruleset}

-- | Updates the board size for rendering purposes. This can be used to react to changes in the UI layout, like resizing the window.
updateSize :: (BoardClass board) =>
    Integer -- | width
 -> Integer -- | height
 -> AppState board
 -> AppState board
updateSize width height state = state {stateWidth=width, stateHeight=height}

-- | Updates the scroll position of the board.
updatePosition :: (BoardClass board) =>
    Integer -- | x coordinate
 -> Integer -- | y coordinate
 -> AppState board
 -> AppState board
updatePosition x y state = state {stateX=x, stateY=y}

-- | Moves the scroll position of the board by dx and dy units on the respective axis.
movePosition :: (BoardClass board) =>
    Integer -- | delta x
 -> Integer -- | delta y
 -> AppState board
 -> AppState board
movePosition dx dy state@(AppState {stateX=x, stateY=y}) = updatePosition (x+dx) (y+dy) state

-- | Toggles a cell between alive and dead.
toggleCell :: (BoardClass board) => Cell -> AppState board -> AppState board
toggleCell cell state@(AppState {stateBoard=board}) = do
    let cells = getCells board
    if cell `member` cells then
        state {stateBoard=setCells board $ delete cell cells}
    else
        state {stateBoard=setCells board $ insert cell cells}

-- | Increases the simulation speed.
increaseSpeed :: (BoardClass board) => AppState board -> AppState board
increaseSpeed state@(AppState {stateTicksBetweenUpdates=ticksBetweenUpdates}) =
    if ticksBetweenUpdates > 1 then
        state {stateTicksBetweenUpdates=ticksBetweenUpdates - 1}
    else
        state

-- | Decreases the simulation speed.
decreaseSpeed :: (BoardClass board) => AppState board -> AppState board
decreaseSpeed state@(AppState {stateTicksBetweenUpdates=ticksBetweenUpdates}) =
    if ticksBetweenUpdates < 10 then
        state {stateTicksBetweenUpdates=ticksBetweenUpdates + 1}
    else
        state

-- | Counts the number of ticks that have passed. This is mainly used to update the simulation with the given speed.
increaseTicks :: (BoardClass board) => AppState board -> AppState board
increaseTicks state@(AppState {stateTicks=ticks}) = state {stateTicks = ticks + 1}