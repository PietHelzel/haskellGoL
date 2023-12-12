-- | This module provides the entrypoint for the interface.
-- The only thing required to get a fully functioning interface is to call the 'runApp' function with a valid initial state.
module UI (runApp) where

import Brick (App(..), neverShowCursor, customMain)
import Brick.BChan (BChan, writeBChan, newBChan)

import Control.Monad (forever)
import Control.Concurrent (forkIO, threadDelay)
import Data.Functor (void)

import Graphics.Vty
import Graphics.Vty.CrossPlatform

import AppState

import UIController
import UIRenderer

import BoardClass

-- | The main Brick app type.
app :: BoardClass board => App (AppState board) CustomEvent ResourceName
app = App {
        appDraw = UIRenderer.drawUI,
        appChooseCursor = neverShowCursor,
        appHandleEvent = UIController.handleEvent,
        appStartEvent = return (),
        appAttrMap = getAttrMap
    }

-- | The entrypoint function for the interface.
runApp :: BoardClass board =>
    AppState board -- | The initial state of the application.
 -> IO (AppState board)
runApp initialState = do
    let builder = mkVty defaultConfig
    initialVty <- builder
    chan <- createTickChannel
    customMain
        initialVty
        builder
        (Just chan)
        app
        initialState

-- | Spawns a seperate thread to continuously send Tick events to the main interface thread.
-- These ticks are then used to update the simulation automatically.
createTickChannel :: IO (BChan CustomEvent)
createTickChannel = do
    chan <- newBChan 10
    let delay = 50000
    writeBChan chan ResizeEvent
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    return chan
