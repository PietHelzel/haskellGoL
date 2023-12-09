module UIController where

import AppState

import Brick (BrickEvent(..), EventM, Extent(Extent), halt)

import BoardClass

import Control.Monad.State.Strict (MonadState(get, put))

import Brick.Main (lookupExtent)

import qualified Graphics.Vty as V

data CustomEvent = Tick
data ResourceName = GameViewport deriving (Eq, Ord)

handleEvent :: BoardClass board => BrickEvent ResourceName CustomEvent -> EventM ResourceName (AppState board) ()
handleEvent (AppEvent Tick) = do
    state <- get
    let state' = updateBoard state
    put state'

handleEvent (VtyEvent (V.EvKey V.KUp [])) = do
    state <- get
    let state' = movePosition 0 (-1) state
    put state'
handleEvent (VtyEvent (V.EvKey V.KDown [])) = do
    state <- get
    let state' = movePosition 0 1 state
    put state'
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = do
    state <- get
    let state' = movePosition (-1) 0 state
    put state'
handleEvent (VtyEvent (V.EvKey V.KRight [])) = do
    state <- get
    let state' = movePosition 1 0 state
    put state'

handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
    state <- get
    return halt state

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