-- | The controller for the user interface. It mainly provides event handlers for all different kinds of user interaction.
module UIController where

import AppState

import Brick (BrickEvent(..), EventM, Extent(Extent), halt)

import BoardClass

import Control.Monad.State.Strict (MonadState(get, put))

import Brick.Main (lookupExtent)

import Cell

import qualified Graphics.Vty as V

-- | Custom events the application can use.
data CustomEvent =
    -- | Used to update the simulation automatically with a specific refresh rate.
    Tick

-- | Custom resources that can be used to identify interface elements.
data ResourceName =
    -- | The viewport that renders the board.
    GameViewport
    deriving (Eq, Ord)

-- | All event handlers are matched on this function
handleEvent :: BoardClass board => BrickEvent ResourceName CustomEvent -> EventM ResourceName (AppState board) ()

-- | Automatically updates the board when the simulation is unpaused, respecting the speed set in the 'AppState'
handleEvent (AppEvent Tick) = do
    state <- get
    let ticks = stateTicks state
    let ticksBetweenUpdates = stateTicksBetweenUpdates state
    if ticks `mod` ticksBetweenUpdates == 0 then do
        let paused = statePaused state
        let state' = if paused then state else updateBoard state
        put $ increaseTicks state'
    else
        put $ increaseTicks state

-- | Scrolls the board upwards when the up arrow key is pressed.
handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    state <- get
    let state' = movePosition 0 (-1) state
    put state'

-- | Scrolls the board downwards when the down arrow key is pressed.
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    state <- get
    let state' = movePosition 0 1 state
    put state'

-- | Scrolls the board to the left when the left arrow key is pressed.
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
    state <- get
    let state' = movePosition (-1) 0 state
    put state'

-- | Scrolls the board to the right when the left arrow key is pressed.
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
    state <- get
    let state' = movePosition 1 0 state
    put state'

-- | Pauses/unpauses the simulation when space is pressed.
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = do
    state <- get
    let state' = togglePaused state
    put state'

-- | Quits the application when q is pressed.
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
    state <- get
    return halt state

-- | Toggles the cell highlighted by the cursor when t is pressed.
handleEvent (VtyEvent (V.EvKey (V.KChar 't') [])) = do
    state <- get
    let x = (stateX state) + (stateWidth state) `div` 2
    let y = (stateY state) + (stateHeight state) `div` 2
    let state' = toggleCell (Cell {x=x, y=y}) state
    put state'

-- | Increases the simulation speed when + is pressed.
handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = do
    state <- get
    let state' = increaseSpeed state
    put state'

-- | Decreases the simulation speed when - is pressed.
handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = do
    state <- get
    let state' = decreaseSpeed state
    put state'

-- | Handles all other events. Currently only used to process window resize events.
handleEvent _ = do
    extents <- lookupExtent GameViewport
    state <- get
    case extents of
        Nothing -> return ()
        Just(Extent _ _ (width, height)) -> do
            let state' = updateSize
                    (toInteger width)
                    (toInteger height)
                    state
            put state'
