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

app :: BoardClass board => App (AppState board) CustomEvent ResourceName
app = App {
        appDraw = UIRenderer.drawUI,
        appChooseCursor = neverShowCursor,
        appHandleEvent = UIController.handleEvent,
        appStartEvent = return (),
        appAttrMap = getAttrMap
    }

runApp :: BoardClass board => AppState board -> IO (AppState board)
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

createTickChannel :: IO (BChan CustomEvent)
createTickChannel = do
    chan <- newBChan 10
    let delay = 100000
    void . forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay delay
    return chan
