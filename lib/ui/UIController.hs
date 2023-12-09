module UIController where

import AppState

import Brick (BrickEvent(..), EventM, Extent(Extent))

import BoardClass

import Control.Monad.State.Strict (MonadState(get, put))

import Brick.Main (lookupExtent)

data CustomEvent = Tick
data ResourceName = GameViewport deriving (Eq, Ord)

handleEvent :: BoardClass board => BrickEvent ResourceName CustomEvent -> EventM ResourceName (AppState board) ()
handleEvent (AppEvent Tick) = do
    state <- get
    let state' = updateBoard state
    put state'

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