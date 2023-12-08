module UIController where

import AppState

import Brick (BrickEvent(..), EventM)

import BoardClass

import Control.Monad.State.Strict (MonadState(get, put))

data CustomEvent = Tick
type ResourceName = ()

handleEvent :: BoardClass board => BrickEvent ResourceName CustomEvent -> EventM ResourceName (AppState board) ()
handleEvent (AppEvent Tick) = do
    state <- get
    let state' = togglePaused state
    put state'

handleEvent event = pure ()
